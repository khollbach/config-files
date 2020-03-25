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

alias ack="rg"
alias cat="bat"

# Neovim, if installed.
if command -v nvim >/dev/null; then
    alias vim="nvim"
fi

# Save a lot of typing.
alias c="_cs"
alias a="ack"
alias ai="ack -i"
alias f="fd"
alias t="my_time"
alias p="python3"
alias v="command nvim"
alias l="command less"
alias r='RANGER_ORIGINAL_PWD=$PWD ranger'
alias ltmk="latexmk -pdf -pvc"
alias pag="ps aux | grep"
alias jq-vim="jq . | v -R - -c 'set syntax=json foldmethod=indent shiftwidth=2' -c 'normal zR'"
alias jv=jq-vim
alias ub=unbuffer
alias ta=tmux_attach

# Make these more user-friendly.
alias which="type -a"
alias grep="grep --color=auto"
alias amm="amm --no-remote-logging"
if command -v rlwrap >/dev/null; then
    alias ocaml="rlwrap ocaml"
fi

# Emacs
alias emacs="emacsclient -a '' -c"
alias remacs="killall emacs; command emacs --daemon"

# Git shorthands.
# Sadly this form breaks commandline autocompletion.
# TODO: fix it somehow. Try one of the suggestions here:
# https://unix.stackexchange.com/questions/4219/how-do-i-get-bash-completion-for-command-aliases
alias g="command git"

alias gs="g s"
alias gsi="g si"
alias gp="g p"
alias gf="g f"
alias gl="g l"
alias gd="g d"
alias gds="g ds"
alias gh="g h"
alias ga="g a"
alias gaa="g aa"
#alias gc="g c"
alias gpu="g pu"
alias gcl="g cl"
alias gco="g co"
alias gll="g ll"
alias glo="g lo"
alias gst="g st"
alias gsp="g sp"
alias gsl="g sl"
alias gb="g b"
alias gr="g r"
alias gcb="g cb"

alias gdh="gd HEAD"
alias gdhh="gd HEAD^"
alias gdhhh="gd HEAD^^"
alias gdhhhh="gd HEAD^^^"
alias gdhhhhh="gd HEAD^^^^"
