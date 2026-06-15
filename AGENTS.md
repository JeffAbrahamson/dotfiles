# Repository Guidelines

* Run `./script/python-checks.sh black` before committing any Python
  changes.
* Run `./script/python-checks.sh flake8` before every commit.
* Run `./script/python-checks.sh pytest` before every commit.
* These checks are intentionally light enough to run routinely, even
  for non-Python changes, because they also catch pre-existing
  breakage.
