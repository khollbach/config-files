#!/bin/sh

# tmux opens every new pane as a login shell, but these are one-time configs.
if [[ -z "$TMUX" ]]; then
    if [[ -f ~/.work-configs/profile ]]; then
        source ~/.work-configs/profile
    fi
fi
