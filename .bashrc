#!/bin/bash

# Different prompt formats.
prompt1='\e[0;1;38;5;${prompt_color}m\W\$\e[0m '
prompt2='\e[0;1;38;5;${prompt_color}m\w\$\e[0m '
prompt3='\e[0m\n\u@\h:\w\n\e[1;38;5;${prompt_color}m\$\e[0m '

prompt_color=9 # Orange

alias ps1='PS1=$prompt1'
alias ps2='PS1=$prompt2'
alias ps3='PS1=$prompt3'

if [[ "$HOSTNAME" == kevan-thinkpad ]]; then
    PS1=$prompt2
else
    PS1=$prompt3
fi

# Default editor for git commit messages, etc.
export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim
export GIT_EDITOR=/usr/bin/vim
export SVN_EDITOR=/usr/bin/vim

# Set LS_COLORS to not use any bold fonts.
eval `dircolors | sed s,01,00,g`
# Make other-writable directory names show as black text on a green background,
# instead of blue text against green (which is unreadable).
export LS_COLORS="${LS_COLORS}ow=30;42:"

# ripgrep config file
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# Load aliases, functions.
source $HOME/.bash_aliases
source $HOME/.bash_functions

# Load work-related defs, etc.
if [[ -f $HOME/notes/config/bashrc-snippet ]]; then
    source $HOME/notes/config/bashrc-snippet
fi
