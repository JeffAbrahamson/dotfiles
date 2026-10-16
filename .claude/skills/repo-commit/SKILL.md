---
name: repo-commit
description: Prepare, propose, or create commits in this repository using its message conventions and required checks.
---

# Repository commits

Follow the authorisation rules in the repository's `AGENTS.md`. Loading
this skill does not authorise a commit or push.

Before proposing or creating a commit:

* Load [repo-testing](../repo-testing/SKILL.md), run `make test` and any
  relevant directory-specific lint/format checks. Never commit with a
  failing check. If a required check cannot run, explain why and ask the
  user to verify it.
* Load [code-review](../code-review/SKILL.md) and resolve the review of
  non-trivial changes before proposing the commit. Typo fixes, comment
  tweaks, and one-line non-logic edits are trivial; treat ambiguous cases
  as non-trivial. Reuse a completed review when no subsequent change
  invalidates it.
* Inspect the diff and stage only the intended change.

These repository conventions take precedence over conflicting generic
commit-skill guidance. Check message lengths before committing; do not
commit first and then amend merely to fix formatting.

## Message style

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
