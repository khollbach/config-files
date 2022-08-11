#!/bin/sh

# Add various bin dirs to PATH.
export PATH=~/.config-files/scripts:$PATH
export PATH=~/.local/bin:$PATH
export PATH=~/bin:$PATH

# Rust.
export PATH=~/.cargo/bin:$PATH

# Go.
export PATH=/usr/local/go/bin:$PATH
if command -v go >/dev/null; then
    export PATH=$(go env GOPATH)/bin:$PATH
fi

# Set editor for git commit messages, etc.
export VISUAL=$(command -v nvim || command -v vim || command -v vi)
export EDITOR=$VISUAL

# Work around a bug in xkb; enable autorepeat for alphabetic keys.
# This gets reset when I unplug and replug my usb keyboard though.
if command -v xset >/dev/null; then
    for scancode in $(seq 24 33) $(seq 38 47) $(seq 52 61) $(seq 152 181); do
        xset r $scancode
    done
fi

# Disable numlock, to turn off the little blue light on my keyboard.
if command -v numlockx >/dev/null; then
    numlockx off
fi

if [[ -f ~/work-configs/profile ]]; then
    source ~/work-configs/profile
fi
