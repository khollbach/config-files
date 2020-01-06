#!/bin/bash

# Ignore .profile if we're in a tmux pane. There's nothing wrong with loading
# it a second time, except that PATH will have a bunch of duplicates, which can
# be annoying to read through. More importantly, running setxkmbap takes a few
# tenths of a second; so when .profile used to load every time, new tmux panes
# would only show a prompt after a short delay. (This is only an issue because
# tmux panes are always opened as a login shell, for some [intended!] reason.)
# https://superuser.com/questions/968942/why-does-tmux-create-new-windows-as-login-shells-by-default
if [[ -f "$HOME/.profile" && -z "$TMUX" ]]; then
    source "$HOME/.profile"
fi

if [[ -f "$HOME/.bashrc" ]]; then
    source "$HOME/.bashrc"
fi
