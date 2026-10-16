# Repository Guidelines

This file is the shared policy for all agents working in this repository.
Read it before starting work. Load the skills below only when their triggers
apply; repository-specific instructions take precedence over generic skills.

## Working rules

* Follow formatter and linter configuration as the source of truth. Avoid
  unrelated whitespace and formatting changes.
* Add comments when they explain why a choice is necessary; don't add
  comments that only narrate the code.
* Use British spelling (en-UK) in code comments and commit messages.
* For new files, follow naming and layout in the target directory and check
  its README, Makefile, and install scripts for inclusion requirements.
  Document any new convention that isn't obvious from nearby code.
* `git add --intent-to-add` new files as soon as they're created, so
  `git status` and `git diff` include them for review.
* Run checks appropriate to the change and review completed changes before
  hand-off, even when no commit was requested. Run `make test` and all
  relevant lint/format checks before every commit; never commit with a
  failing check. If a required check cannot run, explain why and ask the
  user to verify it. Running tests does not require permission.
* Reviews are read-only unless fixes or implementation are authorised.
* Ask before committing unless the user's request already authorises it.
* Destructive Git operations (force-pushes, `reset --hard`, history rewrites,
  or amending shared/pushed commits) require explicit user confirmation.
* Every push requires explicit user authorisation; an earlier push does
  not authorise a later one.
* Perform Git operations with `git`, not `gh`. In particular, merge locally
  with `git merge`, never `gh pr merge`. Use `gh` only for GitHub-specific
  operations, which require separate authorisation.

## Skills to load when applicable

Paths below are relative to the repository root. Read the indicated
`SKILL.md` before the associated work, rather than loading every skill.

* [Testing](.claude/skills/repo-testing/SKILL.md): before running build,
  test, lint, or format checks.
* [Code review](.claude/skills/code-review/SKILL.md): for requested reviews
  and before handing off implemented changes, whether or not committing.
* [Committing](.claude/skills/repo-commit/SKILL.md): before preparing,
  proposing, or creating a commit. Use these repository conventions when
  a generic commit skill differs.
* [Git workflow](.claude/skills/git-workflow/SKILL.md): before Git commands
  targeting another worktree/clone, or any GitHub operation or push.

## Subagents

* Delegate to a subagent when a subtask would pull a lot of disposable
  output into the main context that isn't needed afterward (broad
  searches, file dumps, exploratory research), or when the subtask
  suits a different model — cheap/fast for a narrow well-specified job,
  or more capable for something hard.

* Don't delegate trivial work by default: spawning has overhead and
  loses shared context, so a quick inline grep or read usually beats a
  subagent for a small, well-scoped lookup.
