#!/usr/bin/env bash

# trash-cli doesn't play nice cross-partition
# we'll do it ourselves
# usage: del <file>

set -euo pipefail

trash_loc=$TRASH_LOC

function del_one {
  src=$1

  [ -e "$src" ] || { echo >&2 "Nothing at $src"; exit 1; }

  src_name=$(basename "$(realpath "$src")")
  dest=$trash_loc/$(date +%s.%N-%Y-%m-%d_%H-%M-%S)-$src_name

  mkdir -p "$trash_loc"

  cp -r -- "$src" "$dest"
  [ $? = 0 ] || {
    echo >&2 "Failed to copy $src"
    rm -rf -- "$dest"
    exit 1
  }

  rm -rf -- "$src"
  [ $? = 0 ] || {
    echo >&2 "Failed to delete $src. Partial deletion may have ocurred"
    echo >&2 "A copy of $src was made at $dest"
    exit 1
  }
}

for arg in "$@"; do
  del_one "$arg"
done
