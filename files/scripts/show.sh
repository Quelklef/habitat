#!/usr/bin/env bash

target=${1:-.}; shift

if [ -d "$target" ]; then
    eza "$target" "$@"
elif [ -f "$target" ]; then
    bat "$target" "$@"
else
    echo "Error: $target is not valid"
    exit 1
fi
