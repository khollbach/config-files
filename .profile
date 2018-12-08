#!/bin/sh

# Don't give others permissions to my files.
umask 077

# Only run the following configs if we're not inside a tmux session.
# tmux panes are always opened as a login shell, so .profile gets sourced
# every time.
# It would be fine to run setxkbmap multiple times, except that it's
# noticibly slow, so new panes would only show a prompt after a short delay.
# Also, having directories doubled in your PATH is just annoying.
if [ -z "$TMUX" ]; then
    # Include private bin dirs in path
    export PATH=$HOME/.local/bin:$PATH
    export PATH=$HOME/bin:$PATH

    # Include scripts dir in path
    export PATH=$HOME/config-files/scripts:$PATH

    # Include rust binaries in path
    export PATH=$HOME/.cargo/bin:$PATH

    # setxkbmap will only work if X is running.
    if [ -n "$DISPLAY" ]; then
        # Map capslock to control
        setxkbmap -option ctrl:nocaps
    fi
fi

# Source .bashrc if running bash
if [ -n "$BASH_VERSION" ] && [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi
