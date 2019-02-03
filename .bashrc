#!/bin/bash

function prompt_command {
    local rv=$?

    # Add line breaks if pwd is longer than 50 chars.
    local newline
    local w=$(dirs +0)
    if [ ${#w} -gt 50 ] || [ ${#prompt_contents} -gt 2 ]; then
        newline="\n"
    else
        newline=""
    fi

    local color='\[\e[1;38;5;'"$prompt_color"'m\]'
    local reset='\[\e[0m\]'
    PS1="$reset$newline"
    PS1="$PS1$color$prompt_contents$reset$newline"
    PS1="$PS1$color"'\$'"$reset "
}
PROMPT_COMMAND=prompt_command

prompt_color=9 # Orange

if [[ "$HOSTNAME" == kevan-thinkpad ]]; then
    prompt_contents='\w'
else
    prompt_contents='\u@\h:\w'
fi

# Set LS_COLORS to not use any bold fonts.
eval `dircolors | sed s,01,00,g`

# Make other-writable directory names show as black text on a green background,
# instead of blue text against green (which is unreadable).
export LS_COLORS="${LS_COLORS}ow=30;42:"

# ripgrep config file
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# Git commit messages, etc.
export VISUAL=/usr/bin/vim

# Load aliases, functions.
source "$HOME/.bash_aliases"
source "$HOME/.bash_functions"

# Load work-related defs, etc.
if [[ -f "$HOME/notes/config/bashrc-snippet" ]]; then
    source "$HOME/notes/config/bashrc-snippet"
fi
