#!/bin/bash

echo; echo "==== packages-dev.sh ===="

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get install -y --no-install-recommends      \
    curl                                        \
    fonts-dejavu-core                           \
    fonts-dejavu-mono                           \
    emacs-nox                                   \
    fd-find                                     \
    fonts-linuxlibertine                        \
    fonts-noto-cjk                              \
    fonts-urw-base35                            \
    jq                                          \
    lsb-release                                 \
    lmodern                                     \
    make                                        \
    pandoc                                      \
    poppler-utils                               \
    python3                                     \
    python3-pip                                 \
    python3-venv                                \
    ripgrep                                     \
    rsync                                       \
    shellcheck                                  \
    sway                                        \
    texlive-lang-chinese                        \
    texlive-fonts-recommended                   \
    texlive-xetex                               \

# Install Python testing/linting tools
pip3 install --break-system-packages           \
    black==24.2.0                               \
    flake8                                      \
    pytest                                      \
