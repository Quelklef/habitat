#!/usr/bin/env nix-shell
#!nix-shell -i bash -p xorg.xinput fzf

set -euo pipefail

echo "tune-accel: fine-tune touchpad/mouse acceleration in real time

This lets you iterate on your preferred accel value before
committing it to system config.
"

dev=$(
  xinput list --name-only | while IFS= read -r d; do
    xinput list-props "$d" 2>/dev/null | grep -q "libinput Accel Speed" && echo "$d" || true
  done | fzf --prompt="choose a device > " --height '~30'
)
echo "Using: $dev"

echo "Current accel: $( xinput list-props "$dev" | grep -oP 'libinput Accel Speed(?! \w).*:\s*\K[\d.]+' )"

while read -rp "Set accel (-1 .. 1): " v; do
  xinput set-prop "$dev" "libinput Accel Speed" "$v" || echo "Invalid value"
done
