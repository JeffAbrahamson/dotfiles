#!/bin/bash

## Apply the ufw rules on the hosts whose firewall these dotfiles manage.

## Stop on the first failure: enabling the firewall after a failed
## "allow ssh" could lock us out of a remote host.
set -eu

hostname_short="$(hostname -s)"

case "$hostname_short" in
    birdsong|morning|siegfried|starshine|tau-ceti|vogel|xinghan)
        ;;
    *)
        echo "Not setting ufw firewall rules on this host."
        exit 0
        ;;
esac

echo "Setting up firewall, will require sudo."
if ! sudo -n true 2>/dev/null && ! sudo -v; then
    echo "Warning: sudo not available; skipping firewall setup."
    exit 0
fi

sudo ufw default allow outgoing
sudo ufw default deny incoming
sudo ufw allow ssh/tcp
sudo ufw limit ssh

ufw_enable_status=0
timeout 10 sudo ufw --force enable || ufw_enable_status=$?
if [ "$ufw_enable_status" -eq 124 ]; then
    echo "Error: timed out waiting 10 seconds for ufw enable; firewall setup aborted."
    exit 1
elif [ "$ufw_enable_status" -ne 0 ]; then
    exit "$ufw_enable_status"
fi

if [ "$hostname_short" = siegfried ]; then
    ## For pi-hole:
    sudo ufw allow 80/tcp
    sudo ufw allow 53/udp
    ## For the CUPS print server:
    sudo ufw allow 631/tcp
fi

sudo ufw status verbose
