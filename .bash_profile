#!/bin/bash

# Ignore .profile if we've already run it. There's nothing wrong with loading
# it twice, except that PATH will have a bunch of duplicates, which can be
# annoying to read through. Also, tmux panes are always opened as a login
# shell, and running setxkmbap takes a few tenths of a second; so when .profile
# was configured to load every time, new panes would only show a prompt after a
# short delay.
if [[ -f "$HOME/.profile" && ! "$PATH" =~ "$HOME/config-files/scripts" ]]; then
    source "$HOME/.profile"
fi

if [[ -f "$HOME/.bashrc" ]]; then
    source "$HOME/.bashrc"
fi
