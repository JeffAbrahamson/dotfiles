#!/bin/sh

set -eu

root=$(CDPATH= cd -- "$(dirname "$0")/.." && pwd)
runtime_directory=$(mktemp -d)
trap 'rm -rf "$runtime_directory"' EXIT HUP INT TERM

themes="$runtime_directory/themes"
destination="$runtime_directory/home/.config/bat/config"

printf '%s\n' OneHalfLight 'Solarized (light)' > "$themes"
BAT_THEME_FIXTURE="$themes" \
    "$root/install-config" "$root/bat/config" "$destination" \
    "$root/test/fake-bat"
grep -Fqx -- '--theme="Solarized (light)"' "$destination"

printf '%s\n' OneHalfLight > "$themes"
BAT_THEME_FIXTURE="$themes" \
    "$root/install-config" "$root/bat/config" "$destination" \
    "$root/test/fake-bat"
grep -Fqx -- '--theme="OneHalfLight"' "$destination"

printf '%s\n' Unsupported > "$themes"
if BAT_THEME_FIXTURE="$themes" \
    "$root/install-config" "$root/bat/config" "$destination" \
    "$root/test/fake-bat" 2> "$runtime_directory/error"; then
    echo "Installer succeeded without a supported theme." >&2
    exit 1
fi
grep -Fqx -- '--theme="OneHalfLight"' "$destination"
grep -Fq 'neither Solarized (light) nor OneHalfLight is supported' \
    "$runtime_directory/error"

echo "All bat config installer tests passed."
