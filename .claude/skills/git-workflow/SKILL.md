---
name: git-workflow
description: Safety rules for git worktrees and the git/GitHub boundary in this repo. Load before running git commands that touch another worktree or clone, or before any operation that talks to GitHub (gh, PR merge/close, pushing to a shared branch).
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

Prefer the local equivalent when one exists. For example, use
`git merge --ff-only` rather than `gh pr merge` if the user asked for a
merge and a fast-forward is possible — it does the same thing without
involving GitHub at all. Reach for `gh` only for things that are
genuinely GitHub-only: opening/commenting on PRs and issues, checking
CI status, and similar.

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
