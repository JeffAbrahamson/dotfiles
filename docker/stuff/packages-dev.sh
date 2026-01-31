#!/bin/bash

echo; echo "==== packages-dev.sh ===="

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y                              \
    curl                                        \
    emacs-nox                                   \
    fd-find                                     \
    jq                                          \
    lsb-release                                 \
    python3                                     \
    python3-pip                                 \
    python3-venv                                \
    ripgrep                                     \
    rsync                                       \
    shellcheck                                  \

# Install Python testing/linting tools
pip3 install --break-system-packages           \
    black                                       \
    flake8                                      \
    pytest                                      \
