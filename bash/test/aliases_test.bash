#!/usr/bin/env bash

root_=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
. "${root_}/bash/rc_post/aliases"

failures_=0

fail() {
    echo "FAIL: $*" >&2
    failures_=$((failures_ + 1))
}

assert_equal() {
    local expected_=$1
    local actual_=$2
    local description_=$3

    if [[ $actual_ != "$expected_" ]]; then
        fail "${description_}: expected '${expected_}', got '${actual_}'"
    fi
}

assert_status() {
    local expected_=$1
    local actual_=$2
    local description_=$3

    if (( actual_ != expected_ )); then
        fail "${description_}: expected status ${expected_}, got ${actual_}"
    fi
}

test_seconds_interval() {
    local sleep_calls_=0
    local actual_

    sleep() {
        [[ $1 == 2 ]] || return 2
        sleep_calls_=$((sleep_calls_ + 1))
        (( sleep_calls_ < 2 ))
    }

    actual_=$(seconds 2)
    assert_equal "0  2  " "$actual_" "seconds uses and displays the requested interval"
}

test_seconds_default_interval() {
    local sleep_calls_=0
    local actual_

    sleep() {
        [[ $1 == 1 ]] || return 2
        sleep_calls_=$((sleep_calls_ + 1))
        (( sleep_calls_ < 2 ))
    }

    actual_=$(seconds)
    assert_equal "0  1  " "$actual_" "seconds defaults to an interval of one"
}

test_minutes_interval() {
    local sleep_calls_=0
    local actual_

    sleep() {
        [[ $1 == 120 ]] || return 2
        sleep_calls_=$((sleep_calls_ + 1))
        (( sleep_calls_ < 2 ))
    }

    actual_=$(minutes 2)
    assert_equal "0  2  " "$actual_" "minutes uses and displays the requested interval"
}

test_minutes_default_interval() {
    local sleep_calls_=0
    local actual_

    sleep() {
        [[ $1 == 60 ]] || return 2
        sleep_calls_=$((sleep_calls_ + 1))
        (( sleep_calls_ < 2 ))
    }

    actual_=$(minutes)
    assert_equal "0  1  " "$actual_" "minutes defaults to an interval of one"
}

test_invalid_intervals() {
    local output_
    local status_

    output_=$(seconds 0 2>&1)
    status_=$?
    assert_status 2 "$status_" "seconds rejects zero"
    [[ $output_ == *"integer greater than or equal to 1"* ]] ||
        fail "seconds explains its interval requirement"

    output_=$(minutes 1.5 2>&1)
    status_=$?
    assert_status 2 "$status_" "minutes rejects non-integers"
    [[ $output_ == *"integer greater than or equal to 1"* ]] ||
        fail "minutes explains its interval requirement"

    output_=$(seconds "" 2>&1)
    status_=$?
    assert_status 2 "$status_" "seconds rejects an explicitly empty interval"
}

test_research_directory_helpers() {
    local test_home_
    local starting_directory_
    local output_
    local status_

    # Keep these tests independent of whether fd is installed on the host.
    fdfind() {
        if [[ $* != "--hidden --no-ignore --type f --glob .research" ]]; then
            return 2
        fi
        find . -type f -name .research -printf '%P\n'
    }

    test_home_=$(mktemp -d)
    starting_directory_=$PWD
    mkdir -p \
        "${test_home_}/work/astro/research" \
        "${test_home_}/work/shared-one/research" \
        "${test_home_}/work/shared space/research" \
        "${test_home_}/work/shared-two/research-notes" \
        "${test_home_}/work/talks/research-day"
    touch \
        "${test_home_}/work/astro/research/.research" \
        "${test_home_}/work/shared-one/research/.research" \
        "${test_home_}/work/shared space/research/.research" \
        "${test_home_}/work/shared-two/research-notes/.research"

    output_=$(HOME=$test_home_ resfind)
    [[ $output_ != *"talks"* ]] || fail "resfind ignores unmarked directories named research"
    output_=$(HOME=$test_home_ resfind shared)
    [[ $output_ == *"shared-one/research"* ]] || fail "resfind includes the first substring match"
    [[ $output_ == *"shared-two/research-notes"* ]] || fail "resfind includes the second substring match"

    HOME=$test_home_ rescd astro > "${test_home_}/rescd-output"
    assert_equal "${test_home_}/work/astro/research" "$PWD" "rescd changes to a unique match"
    output_=$(<"${test_home_}/rescd-output")
    assert_equal "$PWD" "$output_" "rescd prints the directory it changes to"

    cd "$starting_directory_" || exit
    HOME=$test_home_ rescd shared one > /dev/null
    assert_equal "${test_home_}/work/shared-one/research" "$PWD" "rescd matches all substrings"

    cd "$starting_directory_" || exit
    HOME=$test_home_ rescd one shared > /dev/null
    assert_equal "${test_home_}/work/shared-one/research" "$PWD" "rescd ignores substring order"

    cd "$starting_directory_" || exit
    output_=$(HOME=$test_home_ rescd shared 2>&1)
    status_=$?
    assert_status 1 "$status_" "rescd rejects ambiguous matches"
    assert_equal "$starting_directory_" "$PWD" "rescd leaves the directory unchanged when ambiguous"
    [[ $output_ == *"Refine with another substring or use a complete path:"* ]] ||
        fail "rescd explains how to refine ambiguous matches"
    [[ $output_ == *"rescd shared-one/research/"* &&
       $output_ == *"rescd shared-two/research-notes/"* ]] ||
        fail "rescd suggests commands for ambiguous matches"
    [[ $output_ == *"rescd shared\\ space/research/"* ]] ||
        fail "rescd shell-quotes suggested paths"

    output_=$(HOME=$test_home_ rescd missing 2>&1)
    status_=$?
    assert_status 1 "$status_" "rescd rejects missing matches"
    assert_equal "$starting_directory_" "$PWD" "rescd leaves the directory unchanged with no match"

    rm -rf "$test_home_"
}

test_research_help() {
    local name_
    local output_
    local status_

    for name_ in resfind rescd resgrep resmake; do
        output_=$("$name_" --help)
        status_=$?
        assert_status 0 "$status_" "$name_ --help succeeds"
        [[ $output_ == "usage: ${name_}"* ]] || fail "$name_ --help starts with usage"
        [[ $output_ == *"See also: "* ]] || fail "$name_ --help has a see-also list"
        [[ ${output_##*See also: } != *"$name_"* ]] ||
            fail "$name_ --help does not list itself under see also"
        [[ ${output_##*See also: } == *allgrep* ]] || fail "$name_ --help refers to allgrep"
        output_=$("$name_" -h)
        [[ $output_ == "usage: ${name_}"* ]] || fail "$name_ -h is the same as --help"
    done
}

test_resmake() {
    local test_home_
    local output_
    local status_

    test_home_=$(mktemp -d)
    mkdir -p "${test_home_}/work/a/notes" "${test_home_}/work/b" "${test_home_}/elsewhere"

    output_=$(cd "${test_home_}/work/a/notes" && HOME=$test_home_ resmake)
    assert_equal "marked: a/notes/" "$output_" "resmake marks the current directory by default"
    [[ -f ${test_home_}/work/a/notes/.research ]] || fail "resmake creates the marker file"

    output_=$(cd "$test_home_" && HOME=$test_home_ resmake work/a/notes work/b)
    status_=$?
    assert_status 0 "$status_" "resmake succeeds for directories under ~/work"
    assert_equal $'already marked: a/notes/\nmarked: b/' "$output_" \
        "resmake reports already-marked directories and marks the rest"

    output_=$(HOME=$test_home_ resmake "${test_home_}/elsewhere" 2>&1)
    status_=$?
    assert_status 1 "$status_" "resmake rejects directories outside ~/work"
    [[ ! -e ${test_home_}/elsewhere/.research ]] || fail "resmake leaves directories outside ~/work unmarked"

    output_=$(HOME=$test_home_ resmake "${test_home_}/work/missing" 2>&1)
    status_=$?
    assert_status 1 "$status_" "resmake rejects missing directories"

    output_=$(HOME=$test_home_ resmake "${test_home_}/work" 2>&1)
    status_=$?
    assert_status 1 "$status_" "resmake rejects ~/work itself"

    rm -rf "$test_home_"
}

test_resgrep() {
    local test_home_
    local output_
    local status_

    resfind() {
        printf '%s\n' "a/research/" "b c/notes/"
    }
    allgrep() {
        printf '%s|' "$PWD" "$@"
    }

    test_home_=$(mktemp -d)
    mkdir -p "${test_home_}/work"

    output_=$(HOME=$test_home_ resgrep -i tea "red tea")
    assert_equal "${test_home_}/work|--dir|a/research/|--dir|b c/notes/|-i|tea|red tea|" "$output_" \
        "resgrep runs allgrep from ~/work over every research directory"

    output_=$(HOME=$test_home_ resgrep 2>&1)
    status_=$?
    assert_status 2 "$status_" "resgrep requires a keyword"

    output_=$(HOME=$test_home_ resgrep -- --help)
    [[ $output_ == *"|--|--help|" ]] || fail "resgrep passes keywords after -- to allgrep"

    resfind() {
        :
    }
    output_=$(HOME=$test_home_ resgrep tea 2>&1)
    status_=$?
    assert_status 1 "$status_" "resgrep fails without research directories"
    [[ $output_ == *resmake* ]] || fail "resgrep suggests resmake without research directories"

    unset -f resfind allgrep
    . "${root_}/bash/rc_post/aliases"
    rm -rf "$test_home_"
}

test_jb_git_status() {
    local test_home_
    local output_
    local repo_

    clear() { :; }
    glt() { echo "glt $*"; }

    test_home_=$(mktemp -d)
    for repo_ in analytics candy-wrapper discovery_ingest jellybooks-ng \
                 production streamer-js-web webpub-viewer; do
        mkdir -p "${test_home_}/src/jellybooks/${repo_}"
    done

    output_=$(HOME=$test_home_ jb-git-status)
    [[ $output_ == "analytics/"$'\n'"glt -1"$'\n\n'"====="* ]] ||
        fail "jb-git-status runs glt -1 by default"
    [[ $output_ == *"webpub-viewer/"$'\n'"glt -1"* ]] ||
        fail "jb-git-status visits every repository"

    output_=$(HOME=$test_home_ jb-git-status 'basename "$PWD"; echo x | tr x y')
    [[ $output_ == "analytics/"$'\n'"analytics"$'\n'"y"$'\n'* ]] ||
        fail "jb-git-status runs a compound payload in each repository"

    rm -rf "$test_home_"
    unset -f clear glt
}

test_seconds_interval
test_seconds_default_interval
test_minutes_interval
test_minutes_default_interval
test_invalid_intervals
test_research_directory_helpers
test_research_help
test_resmake
test_resgrep
test_jb_git_status

if (( failures_ > 0 )); then
    exit 1
fi

echo "All alias tests passed."
