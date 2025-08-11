#!/bin/bash

# Only load configs if Bash is in "interactive" mode.
if [[ $- == *i* ]]; then
    if pushd ~/.config/bash >/dev/null; then
        for file in $(find . -type f); do
            source "$file"
        done
        popd >/dev/null
    fi

    if [[ -f ~/.work-configs/bashrc ]]; then
        source ~/.work-configs/bashrc
    fi
fi
