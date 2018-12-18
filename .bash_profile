#!/bin/bash

# Only source .profile if we're not inside a tmux session.
# tmux panes are always opened as a login shell, so .bash_profile gets sourced
# every time.
# It would be fine to run .profile multiple times, except that setxkbmap is
# noticibly slow, so new panes would only show a prompt after a short delay.
# Also, having directories doubled in your PATH is just annoying.
if [ -z "$TMUX" ] && [ -f "$HOME/.profile" ]; then
    source "$HOME/.profile"
fi

if [ -f "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc"
fi