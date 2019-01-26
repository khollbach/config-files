#!/bin/bash

# Prompt format.
# \e[0;1;38;5;Xm is for bold, X-colored text; \e[0m resets text effects.
export PS1='\[\e[0;1;38;5;${MY_PROMPT_COLOR}m\]\W\$\[\e[0m\] '

# Prompt color.
# On machines other than my laptop; e.g. when ssh'ing:
if [[ "$HOSTNAME" != kevan-thinkpad ]]; then
    # Cyan
    export MY_PROMPT_COLOR=6
else
    # Orange
    export MY_PROMPT_COLOR=9
fi

# Default editor for git commit messages, etc.
export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim
export GIT_EDITOR=/usr/bin/vim
export SVN_EDITOR=/usr/bin/vim

# Custom less options for man.
export MANPAGER=less
# Show search matches on the fourth line from the top instead of at the top.
MANPAGER="$MANPAGER -j4"
# Case insensitive search (this is actually the default in man, but apparently
# gets disabled as soon as you specify a custom pager).
MANPAGER="$MANPAGER -i"

# Custom less options for git output: do clean up screen output after exit, and
# don't quit instantly when the output fits on one screen.
# See https://superuser.com/a/1202694
export GIT_PAGER="less -+X -+F"

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
