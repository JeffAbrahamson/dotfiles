#!/usr/bin/env bash

echo "================ entrypoint ($@) ================"
set -euo pipefail

# Path whose ownership we want to match (bind-mounted source tree)
TARGET_DIR="${TARGET_DIR:-/devsrc}"

# Determine uid/gid from the target dir (numeric)
uid="$(stat -c '%u' "$TARGET_DIR")"
gid="$(stat -c '%g' "$TARGET_DIR")"

# If gid doesn't exist in container, create a group with that gid
if ! getent group "$gid" >/dev/null 2>&1; then
    groupadd -g "$gid" hostgroup
fi

# Ensure /home/dev and any mounted credentials are accessible by target user
chown -R "$uid:$gid" /home/dev 2>/dev/null || true

# If uid doesn't exist, modify dev to that uid/gid
if ! getent passwd "$uid" >/dev/null 2>&1; then
    usermod -u "$uid" -g "$gid" dev
    echo "usermod branch"
    exec env HOME=/home/dev gosu dev "$@"
fi

# uid exists already: use that existing user instead of dev
existing_user="$(getent passwd "$uid" | cut -d: -f1)"
# Ensure that user's primary group matches the target gid if possible
if [ "$(id -g "$existing_user")" != "$gid" ]; then
    usermod -g "$gid" "$existing_user" 2>/dev/null || true
fi
if [[ "$@" = claude ]]; then
    gosu "$existing_user" env HOME=/home/dev /usr/bin/node /usr/local/lib/node_modules/\@anthropic-ai/claude-code/cli.js
elif [[ "$@" = codex ]]; then
    gosu "$existing_user" env HOME=/home/dev /usr/bin/node /usr/local/lib/node_modules/\@openai/codex/bin/codex.js
else
    echo "export HOME=/home/dev if necessary."
    env HOME=/home/dev gosu "$existing_user" "$@"
fi
