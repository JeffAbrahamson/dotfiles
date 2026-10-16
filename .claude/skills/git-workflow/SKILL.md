---
name: git-workflow
description: Safety rules for git worktrees and the git/GitHub boundary in this repo. Load before running git commands that touch another worktree or clone, or before any operation that talks to GitHub (gh, PR/issue operations, or any push).
---

## Worktrees and other clones

Never `cd` into another worktree or clone to run git commands there,
and never chain `cd <dir> && git ...`. That pattern silently runs
against whichever branch that directory happens to have checked out,
which is easy to get wrong and hard to notice. Use `git -C <dir> ...`
instead, or an explicit `git --git-dir=... --work-tree=...` invocation,
so the target is visible in the command itself.

This applies even when no worktrees are active right now — they tend
to get created ad hoc later, and the habit needs to already be in
place.

## git vs. GitHub

Most work in this repo is local git (commit, branch, merge, rebase).
GitHub is a separate system layered on top, and touching it needs
separate authorisation: either the user asked for it directly, or you
asked a clarifying question and got a yes.

Perform Git operations with `git`, never through `gh`. This includes
branching, committing, merging, rebasing, fetching, and pushing. Merge
locally with `git merge` (using `--ff-only` when appropriate); never use
`gh pr merge`, including squash or rebase variants. A merge request does
not authorise a GitHub operation or a push.

Use `gh` only for GitHub-specific operations such as opening or commenting
on PRs and issues, closing an issue, or checking CI status, with the
required authorisation.

## Pushes and destructive operations

Never run `git push` in any form, including to a personal or agent
branch, unless the user explicitly requested the push. Authorization for
one push does not authorize a later push. After a history rewrite, each
push requires explicit authorization; if authorized, use
`git push --force-with-lease`, never `--force` or `-f`.

Get explicit user confirmation before destructive Git operations,
including force-pushes, `reset --hard`, history rewrites, and amending
commits that may already be shared. Treat other potentially destructive
Git operations the same way.
