#!/bin/bash

# CDF stuff.
if [[ "$USER" == hollbac1 ]]; then
    umask 077
    stty erase ^?
    mesg n
    export PATH=$HOME/bin:/usr/local/bin:/usr/bin:/bin:/usr/games
    export MAIL=/var/spool/mail/$USER
    export MAILCHECK=300

    # CDF ignores .profile, so put this here too.
    setxkbmap -option ctrl:nocaps
fi

# Include scripts dir in path.
export PATH=$HOME/config-files/scripts:$PATH

# Set prompt.
# No color.
#export PS1='\W\$ '
# Orange (XTERM 256).
export PS1='\[\e[38;5;166m\]\W\$\[\e[m\] '

# Default editors.
export EDITOR=/usr/bin/vim
export VISUAL=/usr/bin/vim
export GIT_EDITOR=/usr/bin/vim

# Custom ls colors: use dark colors in place of light ones.
# Light colors don't play well with the Solarized color scheme because they all
# get mapped to various shades of gray. If you don't use the Solarized theme,
# comment this line out, since the ANSI dark blue is unreadable against a black
# background.
eval `dircolors | sed "s/01/00/g"`

# Load aliases, functions.
source ~/.bash_aliases
source ~/.bash_functions
