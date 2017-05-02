##!/bin/sh

# Map capslock to control.
setxkbmap -option ctrl:nocaps

# Source bashrc.
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi
