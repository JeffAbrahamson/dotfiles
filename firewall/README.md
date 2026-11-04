# Firewall

This directory contains firewall-related installation support.

## Contents

* [`Makefile`](Makefile) provides the `install` target, which runs `install.sh`.
* [`install.sh`](install.sh) applies the firewall configuration used by this dotfiles setup on the hosts it lists, and does nothing on any other host.
