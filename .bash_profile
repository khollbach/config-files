#!/bin/bash
# This file gets loaded when Bash is run as a "login" shell.

# Load `.profile`, unless this is a tmux pane.
#
# tmux opens every new pane as a login shell, but we don't need to load
# `.profile` a second time.
if [[ -z "$TMUX" ]]; then
    source ~/.profile
fi

# If running in "interactive" mode, load my configs.
#
# E.g., this will be skipped if Bash is invoked as `bash -c some_command`.
if [[ $- == *i* ]]; then
    source ~/.bashrc
fi
