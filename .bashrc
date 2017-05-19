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
    export PATH=$PATH:/usr/games
    export MAIL=/var/spool/mail/$USER
    export MAILCHECK=300
fi

# Set prompt
export PS1='\[\e[1;31m\]\W\$\[\e[m\] '

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

# Load aliases, functions.
source ~/.bash_aliases
source ~/.bash_functions
