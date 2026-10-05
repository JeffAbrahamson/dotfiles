---
name: jeff-doctor
description: Audit this repository against Jeff's current cross-project engineering and AI-agent conventions checklist.
---

# Jeff Doctor

Run the conformity audit described in `/home/jeff/src/jma/AI/jeff-doctor.md`.
Read that file live each time; do not use a vendored or remembered copy.

Before auditing, check whether the checklist clone is current and clean:

1. Run `git -C /home/jeff/src/jma/AI fetch origin main`.
2. Inspect `git -C /home/jeff/src/jma/AI status --short --branch`, its
   current branch, `HEAD`, and `origin/main`.
3. Continue only if the current branch is `main`, `HEAD` equals
   `origin/main`, and the working tree is clean. If fetch fails or any
   condition differs, report the exact state and ask whether to proceed
   against the available local checklist. Do not silently issue a
   conformity report against a stale or locally modified checklist.
4. Once current and clean is confirmed, read the checklist at the path
   above and follow its scope, format, and no-edit requirement.

The checklist may be invoked from Codex by pointing it directly at the
same path. Keep this skill as a live link to that source rather than
copying the checklist into this repository.
