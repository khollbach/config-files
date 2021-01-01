#!/bin/bash

# Ignore .profile if we're in a tmux pane. There's nothing really wrong with
# loading it a second time, except that PATH will have a bunch of duplicates,
# which can be annoying to read through.
if [[ -f "$HOME/.profile" && -z "$TMUX" ]]; then
    source "$HOME/.profile"
fi

if [[ -f "$HOME/.bashrc" ]]; then
    source "$HOME/.bashrc"
fi
