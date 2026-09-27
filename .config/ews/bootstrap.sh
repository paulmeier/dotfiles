#!/bin/sh
# Fetch upstream Emacs Writing Studio into this profile, unmodified.
# Usage: bootstrap.sh [git-ref]   (defaults to the pinned commit below)
set -eu

ref="${1:-6f71755c43cdd1a1bb80fbcace4d3213a0cd868c}"
base="https://raw.githubusercontent.com/pprevos/emacs-writing-studio/$ref"
dir="$(cd "$(dirname "$0")" && pwd)"

for f in init.el ews.el LICENSE; do
  curl -fsSL "$base/$f" -o "$dir/$f.tmp"
  mv "$dir/$f.tmp" "$dir/$f"
done

echo "EWS $ref installed in $dir"
