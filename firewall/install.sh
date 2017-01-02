#!/bin/bash

. ../script/lib.sh

echo "Setting up firewall, will require sudo."
sudo ufw default allow outgoing
sudo ufw default deny incoming
sudo ufw allow ssh/tcp
sudo ufw limit ssh
sudo ufw enable

sudo iptables-save

sudo ufw status verbose
