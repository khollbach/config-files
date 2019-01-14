#!/bin/bash

# Set prompt color / format.
# \e[0;1;38;5;9m is for bold orange text; \e[0m resets text effects.
# See colors scripts for examples.
export PS1='\[\e[0;1;38;5;9m\]\W\$\[\e[0m\] '

# Default editors
export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim
export GIT_EDITOR=/usr/bin/vim
export SVN_EDITOR=/usr/bin/vim

# Custom less options for man.
export MANPAGER=less
# Show search matches on the fourth line from the top instead of at the top.
MANPAGER="$MANPAGER -j4"
# Case insensitive search (this is actually the default in man, but gets
# disabled as soon as you specify a custom pager).
MANPAGER="$MANPAGER -i"

# Set LS_COLORS to not use any bold fonts.
# Ignore this setting if it was already set.
if [[ -z "$LS_COLORS" ]]; then
    eval `dircolors | sed s,01,00,g`
fi

# Make other-writable directory names show as black text on a green background,
# instead of blue text against green (which is unreadable).
export LS_COLORS="${LS_COLORS}ow=30;42:"

# Lynx colors to work well with solarized terminal colors
export LYNX_LSS=$HOME/config-files/lynx-solarized.lss

# ripgrep config file
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# Load aliases, functions.
source ~/.bash_aliases
source ~/.bash_functions

# On machines other than my laptop; e.g. when ssh'ing.
if [[ "$HOSTNAME" != kevan-thinkpad
    && "$HOSTNAME" != kevan-ThinkPad-T450s ]]; then

    # Cyan-colored prompt
    export PS1='\[\e[0;1;38;5;6m\]\W\$\[\e[0m\] '
fi

# Load work-related defs, etc.
if [[ "$HOSTNAME" != kevan-ThinkPad-T450s
    && -f ~/notes/config/bashrc-snippet ]]; then

    source ~/notes/config/bashrc-snippet
fi
