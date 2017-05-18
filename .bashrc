#!/bin/bash

# Work stuff
if [ -f ~/work/bashrc-snippet ]; then
    source ~/work/bashrc-snippet
fi

# CDF stuff
if [ "$USER" == hollbac1 ]; then
    umask 077
    mesg n
    stty erase ^?
    PATH=/usr/local/bin:/usr/bin:/bin:/usr/games
    MAIL=/var/spool/mail/$USER
    MAILCHECK=300
fi

# Modify path if it hasn't been done already.
# TODO: this is brittle; make it loop through the cut output (how to?) instead
#       of the current check.
if [ `echo "$PATH" | cut -d : -f 1` != "$HOME/bin" ]; then
    # Include scripts dir in path
    PATH=$HOME/config-files/scripts:$PATH

    # Include private bin dirs in path
    PATH=$HOME/.local/bin:$PATH
    PATH=$HOME/bin:$PATH
fi

# Set prompt
PS1='\[\e[1;31m\]\W\$\[\e[m\] '

# Default editors
EDITOR=/usr/bin/vim
VISUAL=/usr/bin/vim
GIT_EDITOR=/usr/bin/vim

# Custom less options for man.
# TODO: not working ...
#MANPAGER=less
# Show search matches on the fourth line from the top instead of at the top.
#MANPAGER="$MANPAGER -j4"
# Case insensitive search (this is actually the default in man, but gets
# disabled as soon as you specify a custom pager).
#MANPAGER="$MANPAGER -i"

# Load aliases, functions.
source ~/.bash_aliases
source ~/.bash_functions
