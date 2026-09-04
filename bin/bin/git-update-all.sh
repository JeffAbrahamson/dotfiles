#!/bin/bash

## Assume that $cwd is a directory containing git repositories.
## Update each repository, prune stale remote-tracking references, and
## optionally remove safe local branches whose upstreams disappeared.

usage()
{
    echo "Usage: ${0##*/} [--prune-local]"
}

prune_local=0
case ${1:-} in
    '')
	;;
    --prune-local)
	prune_local=1
	shift
	;;
    -h|--help)
	usage
	exit
	;;
    *)
	usage >&2
	exit 2
	;;
esac

if [ "$#" -ne 0 ]; then
    usage >&2
    exit 2
fi

# Fetch only remote branches into their conventional remote-tracking
# namespace.  An explicit refspec prevents repository configuration from
# directing pruning at local branches, and --no-prune-tags protects local
# tags even when fetch.pruneTags or remote.<name>.pruneTags is enabled.
update-remotes-safely()
{
    local remote refspec failed=0

    while IFS= read -r remote; do
	[ -n "$remote" ] || continue
	refspec="+refs/heads/*:refs/remotes/$remote/*"
	git fetch --no-tags --prune --no-prune-tags --no-write-fetch-head \
	    --refmap= "$remote" "$refspec" || failed=1
    done < <(git remote)

    return "$failed"
}

# Record canonical remote-tracking tips before fetching.  Once fetch prunes a
# ref, its old object ID is no longer available for comparison with the local
# branch.  Also retain the configured upstream details so a concurrent config
# change cannot make a different branch eligible for deletion.
snapshot-upstreams()
{
    local snapshot=$1
    local branch_ref upstream_ref remote remote_ref tracking_ref upstream_oid

    : > "$snapshot"
    while read -r branch_ref upstream_ref remote remote_ref; do
	[ -n "$upstream_ref" ] && [ -n "$remote" ] || continue
	case $remote_ref in
	    refs/heads/*) ;;
	    *) continue ;;
	esac
	tracking_ref="refs/remotes/$remote/${remote_ref#refs/heads/}"
	upstream_oid=$(git rev-parse --verify "$tracking_ref^{commit}" \
	    2>/dev/null) || upstream_oid=-
	printf '%s\t%s\t%s\t%s\t%s\t%s\n' \
	    "$branch_ref" "$upstream_ref" "$remote" "$remote_ref" \
	    "$tracking_ref" "$upstream_oid" \
	    >> "$snapshot"
    done < <(git for-each-ref \
	--format='%(refname) %(upstream) %(upstream:remotename) %(upstream:remoteref)' \
	refs/heads)
}

# Set default_branch and default_tracking_ref.  Refresh the remote's symbolic
# HEAD instead of guessing whether its default branch is main or master.
resolve-default-branch()
{
    local remote=$1 remote_head prefix

    if [ -n "${default_branch_seen[$remote]+yes}" ]; then
	default_branch=${default_branch_cache[$remote]}
	default_tracking_ref=${default_tracking_ref_cache[$remote]}
	[ -n "$default_branch" ]
	return
    fi

    default_branch_seen[$remote]=1
    default_branch=
    default_tracking_ref=
    if git remote set-head "$remote" -a >/dev/null 2>&1; then
	remote_head=$(git symbolic-ref --quiet \
	    "refs/remotes/$remote/HEAD" 2>/dev/null) || remote_head=
	prefix="refs/remotes/$remote/"
	if [[ $remote_head == "$prefix"* ]] &&
		git show-ref --verify --quiet "$remote_head"; then
	    default_branch=${remote_head#"$prefix"}
	    default_tracking_ref=$remote_head
	fi
    fi

    default_branch_cache[$remote]=$default_branch
    default_tracking_ref_cache[$remote]=$default_tracking_ref
    [ -n "$default_branch" ]
}

# Set branch_worktree and branch_worktree_locked for a checked-out branch.
find-branch-worktree()
{
    local wanted_ref=$1 field
    local record_path= record_branch= record_locked=0

    branch_worktree=
    branch_worktree_locked=0
    while IFS= read -r -d '' field; do
	if [ -z "$field" ]; then
	    if [ "$record_branch" = "$wanted_ref" ]; then
		branch_worktree=$record_path
		branch_worktree_locked=$record_locked
		return 0
	    fi
	    record_path=
	    record_branch=
	    record_locked=0
	    continue
	fi
	case $field in
	    'worktree '*) record_path=${field#worktree } ;;
	    'branch '*) record_branch=${field#branch } ;;
	    locked|'locked '*) record_locked=1 ;;
	esac
    done < <(git worktree list --porcelain -z)
    return 1
}

worktree-is-pristine()
{
    local worktree=$1 status

    status=$(git -C "$worktree" status --porcelain \
	--untracked-files=all --ignored 2>/dev/null) || return 1
    [ -z "$status" ]
}

# A known upstream tip must match exactly.  If it was already gone before this
# run, only a branch merged into the remote's default branch is eligible.
branch-is-safe-to-remove()
{
    local branch_ref=$1 remote=$2 upstream_oid=$3 current_oid

    keep_reason=
    current_oid=$(git rev-parse --verify "$branch_ref^{commit}" 2>/dev/null) || {
	keep_reason="it no longer resolves to a commit"
	return 1
    }

    if [ "$upstream_oid" != - ]; then
	if [ "$current_oid" = "$upstream_oid" ]; then
	    return 0
	fi
	keep_reason="its tip differs from the last-fetched upstream"
	return 1
    fi

    if ! resolve-default-branch "$remote"; then
	keep_reason="the default branch for remote '$remote' is unknown"
	return 1
    fi
    if git merge-base --is-ancestor "$branch_ref" \
	    "$default_tracking_ref" 2>/dev/null; then
	return 0
    fi

    keep_reason="its former upstream was already gone and it is not merged"
    keep_reason="$keep_reason into $remote/$default_branch"
    return 1
}

delete-local-branch()
{
    local branch_ref=$1 upstream_ref=$2 remote=$3 remote_ref=$4
    local tracking_ref=$5 upstream_oid=$6 branch=${branch_ref#refs/heads/}
    local current_upstream current_remote current_remote_ref

    read -r current_upstream current_remote current_remote_ref < \
	<(git for-each-ref \
	    --format='%(upstream) %(upstream:remotename) %(upstream:remoteref)' \
	    "$branch_ref")
    if [ "$current_upstream" != "$upstream_ref" ] ||
	    [ "$current_remote" != "$remote" ] ||
	    [ "$current_remote_ref" != "$remote_ref" ] ||
	    git show-ref --verify --quiet "$tracking_ref" ||
	    ! branch-is-safe-to-remove "$branch_ref" "$remote" \
		"$upstream_oid"; then
	echo "Keeping local branch '$branch': its state changed during cleanup." >&2
	return 1
    fi
    git branch -D "$branch"
}

handle-gone-branch()
{
    local branch_ref=$1 upstream_ref=$2 remote=$3 remote_ref=$4
    local tracking_ref=$5 upstream_oid=$6 primary_worktree=$7
    local branch=${branch_ref#refs/heads/}
    local gone_worktree target_worktree

    if ! branch-is-safe-to-remove "$branch_ref" "$remote" "$upstream_oid"; then
	echo "Keeping local branch '$branch': $keep_reason."
	return
    fi

    if ! find-branch-worktree "$branch_ref"; then
	if [ "$prune_local" -eq 0 ]; then
	    echo "Would prune local branch '$branch' (use --prune-local)."
	else
	    delete-local-branch "$branch_ref" "$upstream_ref" "$remote" \
		"$remote_ref" "$tracking_ref" "$upstream_oid"
	fi
	return
    fi

    gone_worktree=$branch_worktree
    if [ "$gone_worktree" != "$primary_worktree" ] &&
	    [ "$branch_worktree_locked" -eq 1 ]; then
	echo "Keeping local branch '$branch':" \
	    "worktree '$gone_worktree' is locked."
	return
    fi
    if ! worktree-is-pristine "$gone_worktree"; then
	echo "Keeping local branch '$branch':" \
	    "worktree '$gone_worktree' is not pristine."
	return
    fi

    if [ "$gone_worktree" != "$primary_worktree" ]; then
	if [ "$prune_local" -eq 0 ]; then
	    echo "Would remove worktree '$gone_worktree' and prune local" \
		"branch '$branch' (use --prune-local)."
	elif git worktree remove "$gone_worktree"; then
	    delete-local-branch "$branch_ref" "$upstream_ref" "$remote" \
		"$remote_ref" "$tracking_ref" "$upstream_oid"
	else
	    echo "Keeping local branch '$branch': could not remove" \
		"worktree '$gone_worktree'." >&2
	fi
	return
    fi

    if ! resolve-default-branch "$remote"; then
	echo "Keeping local branch '$branch': the default branch for" \
	    "remote '$remote' is unknown."
	return
    fi
    if ! git show-ref --verify --quiet "refs/heads/$default_branch"; then
	echo "Keeping local branch '$branch': local default branch" \
	    "'$default_branch' does not exist."
	return
    fi
    if find-branch-worktree "refs/heads/$default_branch"; then
	target_worktree=$branch_worktree
	if [ "$target_worktree" != "$primary_worktree" ]; then
	    echo "Keeping local branch '$branch': default branch" \
		"'$default_branch' is checked out at '$target_worktree'."
	    return
	fi
    fi

    if [ "$prune_local" -eq 0 ]; then
	echo "Would switch '$primary_worktree' to '$default_branch' and" \
	    "prune local branch '$branch' (use --prune-local)."
	return
    fi
    if git switch "$default_branch"; then
	delete-local-branch "$branch_ref" "$upstream_ref" "$remote" \
	    "$remote_ref" "$tracking_ref" "$upstream_oid"
    else
	echo "Keeping local branch '$branch': could not switch to '$default_branch'." >&2
    fi
}

prune-gone-local-branches()
{
    local snapshot=$1 primary_worktree=$2
    local branch_ref upstream_ref remote remote_ref tracking_ref upstream_oid
    local current_upstream current_remote current_remote_ref

    declare -A default_branch_seen=()
    declare -A default_branch_cache=()
    declare -A default_tracking_ref_cache=()

    while IFS=$'\t' read -r branch_ref upstream_ref remote remote_ref \
	    tracking_ref upstream_oid; do
	[ -n "$branch_ref" ] || continue
	[ "$remote" != . ] || continue
	git remote get-url "$remote" >/dev/null 2>&1 || continue
	read -r current_upstream current_remote current_remote_ref < \
	    <(git for-each-ref \
		--format='%(upstream) %(upstream:remotename) %(upstream:remoteref)' \
		"$branch_ref")
	[ "$current_upstream" = "$upstream_ref" ] &&
	    [ "$current_remote" = "$remote" ] &&
	    [ "$current_remote_ref" = "$remote_ref" ] || continue
	git show-ref --verify --quiet "$tracking_ref" && continue
	handle-gone-branch "$branch_ref" "$upstream_ref" "$remote" \
	    "$remote_ref" "$tracking_ref" "$upstream_oid" "$primary_worktree"
    done < "$snapshot"
}

# Fast-forward from the canonical tracking ref populated above.  Avoiding
# git pull prevents configured fetch refspecs or pruning options from changing
# local branches and tags, and --ff-only preserves all local commits.
update-current-branch()
{
    local branch_ref upstream_ref remote remote_ref tracking_ref worktree

    branch_ref=$(git symbolic-ref --quiet HEAD 2>/dev/null) || {
	echo 'Skipping update: HEAD is detached.'
	return
    }
    read -r upstream_ref remote remote_ref < \
	<(git for-each-ref \
	    --format='%(upstream) %(upstream:remotename) %(upstream:remoteref)' \
	    "$branch_ref")
    if [ -z "$upstream_ref" ]; then
	echo 'Skipping update: the current branch has no upstream.'
	return
    fi
    if [ "$remote" = . ] || [[ $remote_ref != refs/heads/* ]]; then
	echo 'Skipping update: the current branch does not track a remote branch.'
	return
    fi

    tracking_ref="refs/remotes/$remote/${remote_ref#refs/heads/}"
    if ! git show-ref --verify --quiet "$tracking_ref"; then
	echo "Skipping update: the current branch's upstream is gone."
	return
    fi
    worktree=$(git rev-parse --show-toplevel 2>/dev/null) || {
	echo 'Skipping update: could not find the current worktree.'
	return
    }
    if ! worktree-is-pristine "$worktree"; then
	echo 'Skipping update: the current worktree is not pristine.'
	return
    fi
    git merge --ff-only "$tracking_ref"
}

update-repository()
(
    local directory=$1 snapshot primary_worktree

    cd "$directory" || exit
    [ -d .git ] || exit

    echo "==> $directory <=="
    snapshot=$(mktemp /tmp/git-update-all-upstreams_XXXXXX) || exit
    trap 'rm -f "$snapshot"' EXIT
    snapshot-upstreams "$snapshot"
    primary_worktree=$(git rev-parse --show-toplevel) || exit

    if update-remotes-safely; then
	prune-gone-local-branches "$snapshot" "$primary_worktree"
    else
	echo 'Skipping local branch cleanup because remote update failed.' >&2
    fi
    echo
    update-current-branch
    echo
    git status
)

first=1
for directory in *; do
    [ -d "$directory/.git" ] || continue
    if [ "$first" -eq 0 ]; then
	echo
	echo ============================================================
	echo
    fi
    first=0
    update-repository "$directory"
done
