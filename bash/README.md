# Bash

This directory holds the shell startup files and the fragments they are assembled from. The layout separates login, interactive, and logout behavior so changes stay localized and easier to reason about.

## Contents

* [`bash/`](bash/README.md) contains the actual shell fragments and generated include files.
* [`bash_logout-include`](bash_logout-include), [`bash_profile-include`](bash_profile-include), and [`bashrc-include`](bashrc-include) are the top-level files installed into the home directory.
* [`Makefile`](Makefile) provides the `install` and `test` targets for the bash configuration.
