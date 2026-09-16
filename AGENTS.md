# Repository Guidelines

(Commit message format is in `CLAUDE.md`.)

## Environment

* Checks can run on the host or in Docker (`docker/docker-manage.sh
  test`, which runs `make test` inside the `dev-test` service). Tell
  them apart before running build/test commands: inside the container
  the repo lives at `/devsrc`; on the host it's the normal deep path
  under your home directory. State which one you're in, and how you
  told, before running build/test commands.

## Testing

* Run `make test` before every commit, even for changes that look
  unrelated — it's cheap and catches pre-existing breakage too. It runs
  the Python checks below plus every subdirectory's own test target
  (`bash`, `bat`, `bin`, `emacs`, `git`, `i3`, `mutt`, `notion`,
  `ratpoison`, `sway`; see `Makefile`).

* For fast iteration on Python-only changes, the individual checks:
  * `./script/python-checks.sh black`
  * `./script/python-checks.sh flake8`
  * `./script/python-checks.sh pytest`

* Running tests doesn't need to wait for permission — just run them.

* Never commit with a failing check. If a check can't be run (tool
  unavailable, etc.), say so explicitly and ask the user to verify
  rather than skipping silently.

## Subagents

* Delegate to a subagent when a subtask would pull a lot of disposable
  output into the main context that isn't needed afterward (broad
  searches, file dumps, exploratory research), or when the subtask
  suits a different model — cheap/fast for a narrow well-specified job,
  or more capable for something hard.

* Don't delegate trivial work by default: spawning has overhead and
  loses shared context, so a quick inline grep or read usually beats a
  subagent for a small, well-scoped lookup.
