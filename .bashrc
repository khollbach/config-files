#!/bin/bash

# Quick hack to put the initial prompt at the bottom of the screen.
#
# Only run once per shell; i.e. not when I reload bashrc.
if [[ -z "$BASHRC_LOADED" ]]; then
    # Print 100 newlines.
    for i in $(seq 1 100); do
        echo
    done
fi
BASHRC_LOADED=true

# Customize the prompt; load functions and aliases; misc configs.
if pushd ~/.config/bash >/dev/null; then
    for file in $(find . -type f); do
        source "$file"
    done
    popd >/dev/null
fi

if [[ -f ~/notes/configs/bashrc ]]; then
    source ~/notes/configs/bashrc
fi
