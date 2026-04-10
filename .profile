#!/bin/sh

source ~/.config/environment.d/env.conf

# tmux opens every new pane as a login shell, but these are one-time configs.
if [[ -z "$TMUX" ]]; then
    if [[ -f ~/.work-configs/profile ]]; then
        source ~/.work-configs/profile
    fi

    # Load NVM.
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
fi
