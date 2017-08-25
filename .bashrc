#!/bin/bash

# CDF stuff
if [ "$USER" == hollbac1 ]; then
    # Source .profile, since CDF ignores it.
    # There's a guard in .profile to make sure it doesn't source .bashrc when
    # on CDF, to avoid an infinite loop.
    source ~/.profile

    umask 077
    mesg n
    stty erase ^?
    export PATH=$PATH:/usr/games
    export MAIL=/var/spool/mail/$USER
    export MAILCHECK=300
fi

# Set prompt
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
if [ -z "$LS_COLORS" ]; then
    eval `dircolors | sed s,01,00,g`
fi

# Lynx colors
export LYNX_LSS=$HOME/config-files/lynx.lss

# Load aliases, functions.
source ~/.bash_aliases
source ~/.bash_functions
