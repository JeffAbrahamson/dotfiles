# Commit messages

(Build/test commands are in `AGENTS.md`.)

## Style

- Subject: imperative present tense, no trailing period, ~50 characters.
- Blank line between subject and body.
- Body wraps at ~72-78 columns, except where wrapping would hurt (code,
  URLs, tables).
- Body explains *why* — motivation, approach, tradeoffs — not a
  restatement of what the diff already shows.
- Multi-line messages use a heredoc or `-F`, never a literal `\n` inside
  an `-m` string.
- Check line lengths in the drafted message before proposing it — don't
  commit first and fix with `--amend` afterward.
- No emojis. AI co-authorship, if credited, is one plain trailer line,
  not a decorative footer.
- One logical change per commit.

## Bullet formatting

- Bullet lists are indented two spaces.
- Single-line bullet items use `-`.
- If any bullet item wraps to multiple lines, prefer `*` over `-` for all
  items in that list, and include a blank line between items.

## Before committing

- Run `make test` first (see `AGENTS.md`); never commit with a failing
  test.
- For any non-trivial change, run an independent review pass (e.g. the
  `code-review` skill) before proposing the commit. Trivial exceptions:
  typo fixes, comment tweaks, one-line non-logic edits. Treat anything
  ambiguous as non-trivial.
- Ask before committing by default. Committing without asking is fine
  only when the user's request already covers it end-to-end (e.g. "make
  this change and commit it").
- Destructive git operations (force-push, `reset --hard`, history
  rewrites, amending a shared/pushed commit) always require explicit
  user confirmation, regardless of the above.
- `git add --intent-to-add` new files as soon as they're created, so
  `git status`/`git diff` show them for review instead of leaving them
  as easy-to-miss untracked files.
