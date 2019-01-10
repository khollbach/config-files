#!/bin/bash

# Set prompt.
# \e[0;1;38;5;9m is for orange text; \e[0m resets text effects.
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

# Set LS_COLORS to use dark colors in place of light ones,
# but only if it wasn't set already.
# This is to play well with 'Solarized' terminal colorscheme, where the light
# colors are replaced with various shades of greyscale for use in, e.g.,
# Vim colorschemes.
if [[ -z "$LS_COLORS" ]]; then
    eval `dircolors | sed s,01,00,g`
fi

# Lynx colors to work well with solarized terminal colors
export LYNX_LSS=$HOME/config-files/lynx-solarized.lss

# ripgrep config file
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# Load aliases, functions.
source ~/.bash_aliases
source ~/.bash_functions

# Load work-related defs, etc.
if [[ `hostname` != kevan-ThinkPad-T450s
    && -f ~/notes/scripts/bashrc_snippet ]]; then

    source ~/notes/scripts/bashrc_snippet
fi
