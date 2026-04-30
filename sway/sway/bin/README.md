# Sway Helper Scripts

This directory contains scripts used by the Sway environment for idle handling, DPMS control, network state, and power notifications.

## Notable scripts

* [`sway-desktop-dpms-off`](sway-desktop-dpms-off), [`sway-desktop-dpms-on`](sway-desktop-dpms-on), and the [`sway-desktop-idle-*`](sway-desktop-idle-active) scripts coordinate display power and idle behavior.
* [`sway-desktop-utils.sh`](sway-desktop-utils.sh) is the shared shell glue used by several of the desktop-management scripts.
* [`sway-network-status`](sway-network-status), [`sway-power-monitor`](sway-power-monitor), and [`sway-power-status`](sway-power-status) feed status and notification information back into the desktop session.
