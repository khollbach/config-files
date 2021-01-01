#!/bin/sh

# Include private bin dirs, scripts dir, rust binaries in path.
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="$HOME/config-files/scripts:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

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
