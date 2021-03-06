#!/bin/sh

# Include private bin dirs, scripts dir, rust and go binaries in path.
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/config-files/scripts:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="/usr/local/go/bin:$PATH"
if command -v go >/dev/null; then
    export PATH="$(go env GOPATH)/bin:$PATH"
fi

# Set default editor for git commit messages, ranger, etc.
export VISUAL="$(
    if command -v nvim >/dev/null; then
        command -v nvim
    elif command -v vim >/dev/null; then
        command -v vim
    else
        command -v vi
    fi)"
export EDITOR="$VISUAL"

# Pager, mostly for ranger.
export PAGER="$(
    if command -v bat >/dev/null; then
        command -v bat_less
    else
        command -v less
    fi)"

# Work around a bug in xkb; enable autorepeat for alphabetic keys.
for scancode in $(seq 24 33) $(seq 38 47) $(seq 52 61) $(seq 152 181); do
    xset r $scancode
done
