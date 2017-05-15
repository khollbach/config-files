#!/bin/bash

# Work stuff.
if [[ -f ~/work/bashrc-snippet ]]; then
    source ~/work/bashrc-snippet
fi

# CDF stuff.
if [[ "$USER" == hollbac1 ]]; then
    umask 077
    stty erase ^?
    mesg n
    PATH=/usr/local/bin:/usr/bin:/bin:/usr/games
    MAIL=/var/spool/mail/$USER
    MAILCHECK=300

    # CDF ignores .profile, so put this here too.
    setxkbmap -option ctrl:nocaps
fi

# Include scripts dir in path.
PATH=$HOME/config-files/scripts:$PATH

# Set prompt.
# No color.
#PS1='\W\$ '
# Orange (XTERM 256).
PS1='\[\e[38;5;166m\]\W\$\[\e[m\] '

# Default editors.
EDITOR=/usr/bin/vim
VISUAL=/usr/bin/vim
GIT_EDITOR=/usr/bin/vim

# Custom ls colors: use dark colors in place of light ones.
# Light colors don't play well with the Solarized color scheme because they all
# get mapped to various shades of gray. If you don't use the Solarized theme,
# comment this line out, since the ANSI dark blue is unreadable against a black
# background.
eval `dircolors | sed "s/01/00/g"`

# Lynx colors.
LYNX_LSS=$HOME/config-files/lynx.lss

# Custom less options for man.
MANPAGER=less
# Show search matches on the fourth line from the top instead of at the top.
MANPAGER="$MANPAGER -j4"
# Case insensitive search (this is actually the default in man, but gets
# disabled as soon as you specify a custom pager).
MANPAGER="$MANPAGER -i"

# Load aliases, functions.
source ~/.bash_aliases
source ~/.bash_functions
