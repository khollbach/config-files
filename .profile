#!/bin/sh

# Include private bin dirs in path
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/bin:$PATH

# Include scripts dir in path
export PATH=$HOME/config-files/scripts:$PATH

# Rust binaries
export PATH=$HOME/.cargo/bin:$PATH

# setxkbmap will only work if X is running.
if [ -n "$DISPLAY" ] && command -v setxkbmap >/dev/null; then
    # Map capslock to control
    setxkbmap -option ctrl:nocaps

    # Swap alt and super on Mac keyboard, since they're backwards.
    if [ "$HOSTNAME" == kevan-MacBook ] &&
        [ -f "$HOME/config-files/swap-alt-super.xmodmap" ]; then

        xmodmap "$HOME/config-files/swap-alt-super.xmodmap"
    fi
fi
