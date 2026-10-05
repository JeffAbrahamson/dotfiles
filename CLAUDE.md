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
- If the branch name contains a plausible issue number, include a
  reference such as `Part of #123`; use judgement in long-lived
  repositories or where the number is implausible, and ask if uncertain.
- Put issue and incident references at the end using a consistent form,
  such as `Refs: #123` or `Part of #123`.
- No emojis. AI co-authorship, if credited, is one plain trailer line,
  not a decorative footer.
- One logical change per commit.
- Use British spelling (en-UK) in commit messages and code comments.

## Bullet formatting

- Bullet lists are indented two spaces.
- Single-line bullet items use `-`.
- If any bullet item wraps to multiple lines, prefer `*` over `-` for all
  items in that list, and include a blank line between items.

## Before committing

* Run `make test` first (see `AGENTS.md`); never commit with a failing
  test.

* For any non-trivial change, run an independent review pass (e.g. the
  `code-review` skill) before proposing the commit. Trivial exceptions:
  typo fixes, comment tweaks, one-line non-logic edits. Treat anything
  ambiguous as non-trivial.

* During implementation, do a light review of the changed code to catch
  obvious mistakes. Near hand-off, do a focused, independent review of
  the completed diff and the surrounding code needed to check suspected
  issues. Use parallel focused reviews when their independent coverage
  justifies the overhead; don't repeat a full review on every edit.

* Announce each review pass out loud (e.g. "launching a second review
  pass"), say when you are waiting for it, and report what it found and
  how it was addressed — not a silent pass/fail. Count each reviewer
  launch explicitly.

* Treat a review as unresolved until it comes back clean, with only
  trivial issues, or with remaining issues explicitly and deliberately
  left unaddressed. Fix substantive issues and get a fresh review of the
  fixes. Apply the same judgement to human and agent feedback: explain
  disagreements instead of silently complying or ignoring them.

* If review/fix cycles go past about three rounds, stop and ask the
  user rather than continuing to iterate.

* Ask before committing by default. Committing without asking is fine
  only when the user's request already covers it end-to-end (e.g. "make
  this change and commit it").

* Reviews are read-only by default: report findings without editing,
  fixing, or committing unless separately asked.

* Destructive git operations (force-push, `reset --hard`, history
  rewrites, amending a shared/pushed commit) always require explicit
  user confirmation, regardless of the above.

* `git add --intent-to-add` new files as soon as they're created, so
  `git status`/`git diff` show them for review instead of leaving them
  as easy-to-miss untracked files.

* Read `.claude/skills/git-workflow/SKILL.md` before Git commands that
  touch another worktree or clone, or that interact with GitHub. It
  covers worktree safety, pushes, and GitHub authorization.

## Review findings

* Review the diff and only the surrounding code needed to verify a
  suspected issue. Don't report pre-existing problems, issues on
  unchanged lines, lint/type/test failures that the project's checks
  already catch, pedantic preferences, or issues explicitly silenced in
  code. Run checks directly only when CI doesn't cover them.
* Explain the triggering condition and its impact for each finding.
  Rank findings by severity and confidence, and cite precise file and
  line locations. When reporting outside the working tree, include a
  stable commit reference.
* If the review finds no issues, say so explicitly.
