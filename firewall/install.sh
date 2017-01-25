#!/bin/bash

. ../script/lib.sh

echo "Setting up firewall, will require sudo."
sudo ufw default allow outgoing
sudo ufw default deny incoming
sudo ufw allow ssh/tcp
sudo ufw limit ssh
sudo ufw enable

## If we want to see the current iptables rules, we could call
## iptables-save.  Note that this doesn't actually persist the state.
# sudo iptables-save

sudo ufw status verbose
