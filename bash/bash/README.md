# Bash Fragments

This subtree contains the modular pieces that build the final shell configuration. It is useful when you want to change one phase of shell startup without editing a monolithic `.bashrc`.

## Contents

* [`activate.sh`](activate.sh) contains environment activation helpers.
* [`profile`](profile), [`rc`](rc), and [`logout`](logout) are the core entry fragments for login shells, interactive shells, and logout handling.
* [`profile_pre/`](profile_pre), [`profile_post/`](profile_post), [`rc_pre/`](rc_pre), and [`rc_post/`](rc_post) hold ordered hook snippets.

## Notable pieces

* [`rc_post/prompt`](rc_post/prompt) defines the interactive prompt and shell ergonomics.
* [`rc_post/git-worktrees`](rc_post/git-worktrees) and [`rc_post/git-prompt`](rc_post/git-prompt) add git-aware shell behavior.
* [`rc_post/aliases`](rc_post/aliases) and [`rc_post/rsync`](rc_post/rsync) are small, focused convenience layers.
