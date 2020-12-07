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

# setxkbmap and xmodmap will only work if X is running.
if [ -n "$DISPLAY" ]; then
    # Map capslock to control.
    if command -v setxkbmap >/dev/null; then
        setxkbmap -option ctrl:nocaps
    fi

    # Workman keyboard layout.
    if command -v xmodmap >/dev/null && [ -f "$HOME/config-files/workman.xmodmap" ]; then
        xmodmap "$HOME/config-files/workman.xmodmap"
    fi

    # Swap alt and super on Mac keyboard, since they're backwards.
    if [ "$HOSTNAME" == kevan-MacBook ] &&
        command -v xmodmap >/dev/null && [ -f "$HOME/config-files/swap-alt-super.xmodmap" ]; then

        xmodmap "$HOME/config-files/swap-alt-super.xmodmap"
    fi
fi
