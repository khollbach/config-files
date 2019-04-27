#!/bin/sh

# Include private bin dirs in path
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/bin:$PATH

# Include scripts dir in path
export PATH=$HOME/config-files/scripts:$PATH

# Rust binaries
export PATH=$HOME/.cargo/bin:$PATH

# Neovim
export PATH=$HOME/.opt/nvim-linux64/bin:$PATH

# setxkbmap will only work if X is running.
if [ -n "$DISPLAY" ] && command -v setxkbmap >/dev/null; then
    # Map capslock to control
    setxkbmap -option ctrl:nocaps

    # Swap alt and super on Mac keyboard, since they're backwards.
    if [ "$HOSTNAME" == kevan-MacBook ] &&
        [ -f "$HOME/config-files/mac-swap-alt-super.xmodmap" ]; then

        xmodmap "$HOME/config-files/mac-swap-alt-super.xmodmap"
    fi
fi

# Start redshift if it isn't already running.
# Currently I have the issue that it doesn't terminate when I log out.
# And thus when I log back in after, the screen won't be red-shifted, because
# the old instance is still running but isn't affecting the current session.
# (Manual fix is to `killall redshift; redshift &`)
if [ -n "$DISPLAY" ] && command -v redshift >/dev/null &&
    [ -z "`ps aux | grep redshift | grep -v grep`" ]; then

    redshift > /dev/null 2>&1 &
fi
