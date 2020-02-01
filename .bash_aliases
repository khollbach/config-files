#!/bin/bash

# Edit / update configs.
alias co="c ~/config-files"
alias u="source ~/config-files/update_configs"

# ls aliases
alias ls="ls -vh --color=auto"
alias la="ls -A"
alias ll="ls -lG"
alias lla="ll -lA"
alias lt="ll -tr"
alias lta="lt -A"

# `-` = `c -`
alias -- -="c -"

# `..` = `c ..`, etc...
alias      ..="c .."
alias     ...="c ../.."
alias    ....="c ../../.."
alias   .....="c ../../../.."
alias  ......="c ../../../../.."
alias .......="c ../../../../../.."

# ack -> rg
alias ack="ripgrep.rg"

# Neovim, if installed.
if command -v nvim >/dev/null; then
    alias vim="nvim"
fi

# Save a lot of typing.
alias c="_cs"
alias a="ack"
alias ai="ack -i"
alias f="my_find"  # TODO: implement these in terms of `rg` so you can take
alias fn="find . -name"  # advantage of ignoring node_modules, .git, etc.
alias t="my_time"
alias p="python3"
alias v="command nvim"
alias l="command less"
alias r="ranger"
alias ltmk="latexmk -pdf -pvc"
alias pag="ps aux | grep"
alias hig="history | cut -c 8- | uniq | grep"  # I mostly use C-r now.

# Unlearn muscle memory.
alias cs="sl"
alias vi="sl"
alias vim="sl"
alias git="sl"
alias less="sl"
alias gss="sl"
alias glh="sl"

# Make these more user-friendly.
alias which="type -a"
alias grep="grep --color=auto"
alias amm="amm --no-remote-logging"
if command -v rlwrap >/dev/null; then
    alias ocaml="rlwrap ocaml"
fi
alias jq="jq -C ."  # Colors always, identity filter.

# Emacs
alias emacs="emacsclient -a '' -c"
alias remacs="killall emacs; command emacs --daemon"

# Misc.
#alias sl="sl -e"
#alias LS="LS -e"
alias rain="rain -d 150"

# Git shorthands.
# Sadly this form breaks commandline autocompletion.
# TODO: fix it somehow. Try one of the suggestions here:
# https://unix.stackexchange.com/questions/4219/how-do-i-get-bash-completion-for-command-aliases
alias g="command git"

alias gs="g s"
alias gl="g l"
alias gd="g d"

alias gst="g st"
alias gsp="g st pop"
alias gsl="g st list"

alias gdh="gd HEAD"
alias gdhh="gd HEAD^"
alias gdhhh="gd HEAD^^"
alias gdhhhh="gd HEAD^^^"
alias gdhhhhh="gd HEAD^^^^"
