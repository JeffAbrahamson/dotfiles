#!/bin/bash

if [ "X$HOSTNAME" != Xbirdsong -a "X$HOSTNAME" != Xstarshine ]; then
    echo "Not setting ufw firewall rules on this host."
    exit 0
fi


. ../script/lib.sh

echo "Setting up firewall, will require sudo."
sudo ufw default allow outgoing
sudo ufw default deny incoming
sudo ufw allow ssh/tcp
#sudo ufw allow 5353/udp		# For hp-5540 printer discovery (?)
sudo ufw limit ssh
sudo ufw enable
if [ "X$$HOSTNAME" = "Xsiegfried" ]; then
    ## AFP no longer works with Stéphane's iMac (12/2020)
    ## and he's not willing to help fix it (taking the
    ## time to try things and explore what might work).
    ## So deactivating, 1/2021.
    # sudo ufw allow afpovertcp/tcp    # 548/tcp
    ## For pi-hole:
    sudo ufw allow 80/tcp
    sudo ufw allow 53/udp    # DNS
fi

## If we want to see the current iptables rules, we could call
## iptables-save.  Note that this doesn't actually persist the state.
# sudo iptables-save

sudo ufw status verbose
