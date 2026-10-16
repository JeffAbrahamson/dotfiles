---
name: code-review
description: Review repository changes on request or before handing off implemented changes, including changes that will not be committed.
---

# Code review

Reviews requested on their own are read-only unless fixes are separately
authorised. For implementation tasks, fix issues within the authorised
scope.

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

For non-trivial changes, make the final pass independent of the
implementation pass: inspect the completed diff afresh, or use a separate
reviewer when justified. A separate agent is not required for every review.

## Findings

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
