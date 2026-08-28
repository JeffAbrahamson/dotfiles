# Repository Guidelines

(Commit message format is in `CLAUDE.md`.)

## Environment

* Checks can run on the host or in Docker (`docker/docker-manage.sh
  test`, which runs `make test` inside the `dev-test` service). Tell
  them apart before running build/test commands: inside the container
  the repo lives at `/devsrc`; on the host it's the normal deep path
  under your home directory.

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
* Never commit with a failing check. If a check can't be run (tool
  unavailable, etc.), say so explicitly and ask the user to verify
  rather than skipping silently.
