#!/usr/bin/env bash

root_=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
. "${root_}/bash/rc_post/docker"
failures_=0

fail() {
    echo "FAIL: $*" >&2
    failures_=$((failures_ + 1))
}

# Mock Docker so tests never prune the real cache.
docker() {
    case "$1 $2" in
        'system df')
            [[ $# == 2 ]] || return 99
            if [[ ${df_status_:-0} != 0 ]]; then
                echo 'Docker disk usage failed' >&2
                return "$df_status_"
            fi
            echo 'TYPE            TOTAL     ACTIVE    SIZE      RECLAIMABLE'
            echo 'Images          10        2         5GB       3GB'
            echo 'Containers      2         2         1GB       0B'
            echo 'Local Volumes   3         0         2GB       2GB'
            echo 'Build Cache     495       0         60GB      60GB'
            ;;
        'builder prune')
            printf 'docker'
            printf ' <%s>' "$@"
            echo
            return "${prune_status_:-0}"
            ;;
        *) return 99 ;;
    esac
}

output_=$(docker-cleanup)
status_=$?
expected_='docker-cleanup: size=50GB age=336h
Before age-based cleanup:
  Build Cache     495       0         60GB      60GB
  docker <builder> <prune> <--all> <--force> <--filter> <until=336h>
Before size-based cleanup:
  Build Cache     495       0         60GB      60GB
  docker <builder> <prune> <--all> <--force> <--keep-storage> <50GB>
At exit:
  Build Cache     495       0         60GB      60GB'
[[ $status_ == 0 && $output_ == "$expected_" ]] || fail 'defaults, prune order, and three cache reports'

for age_ in 1 '1 week' 7d 168h; do
    output_=$(docker-cleanup --age "$age_" --size 20)
    [[ $? == 0 && $output_ == *'size=20GB age=168h'* &&
       $output_ == *'<until=168h>'* && $output_ == *'<20GB>'* ]] || fail "overrides with age $age_"
done

output_=$(docker-cleanup --size '2.5 GB' --age '2 days')
[[ $? == 0 && $output_ == *'size=2.5GB age=48h'* ]] || fail 'explicit units and spaces'

for args_ in '--size' '--age' '--size invalid' '--age invalid' '--unknown' '--age --size'; do
    read -r -a arguments_ <<< "$args_"
    output_=$(docker-cleanup "${arguments_[@]}" 2>&1)
    [[ $? == 2 && $output_ != *'Build Cache'* && $output_ != *'<prune>'* ]] || fail "invalid arguments: $args_"
done

prune_status_=7
output_=$(docker-cleanup)
[[ $? == 7 && $output_ == *$'At exit:\n  Build Cache     495       0         60GB      60GB' &&
   $output_ != *'Before size-based cleanup:'* ]] || fail 'failed prune stops cleanup, reports final size, and preserves status'

df_status_=9
output_=$(docker-cleanup 2>&1)
[[ $? != 0 && $output_ == *'Docker disk usage failed'* &&
   $output_ == *'At exit:'* && $output_ != *'<prune>'* ]] || fail 'disk usage failure stops cleanup and remains visible'

if (( failures_ )); then
    exit 1
fi
echo 'All Docker helper tests passed.'
