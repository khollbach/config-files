#!/bin/bash

# Update configs.
alias u="source ~/config-files/update_configs"
alias toggle-colors="command toggle-colors && source ~/.bashrc"

# ls aliases.
alias ls="ls -vh --color=auto"
alias la="ls -A"
alias ll="ls -lG"
alias lla="ll -lA"
alias lt="ll -tr"
alias lta="lt -A"

# `-`: change to previous directory.
# `..`: change to parent directory.
alias -- -="c -"
alias      ..="c .."
alias     ...="c ../.."
alias    ....="c ../../.."
alias   .....="c ../../../.."
alias  ......="c ../../../../.."
alias .......="c ../../../../../.."

# Spend less time typing.
alias c=cs
alias v=tmux_nvim
alias vv='tmux_nvim -c "call ToggleDecorations()"'
alias l=bat_less
alias a=ack
alias ae="ack --case-sensitive"
alias aw="ack --case-sensitive -w"
alias f="fd --hidden --exclude .git"
alias r='PAGER=bat_less ranger_pwd=$PWD ranger'
alias p=python3
alias k=cargo
alias kc="kk c"
alias kb="kk b"
alias kt="kk t"
alias kr="k r"
alias t="my_time "
alias ub="unbuffer "
alias ta=tmux_attach
alias pag="ps aux | grep"
alias ltmk="latexmk -pdf -pvc --shell-escape"
alias jq-vim="jq . | nvim -R - -c 'set syntax=json foldmethod=indent shiftwidth=2' -c 'normal zR'"
alias jv=jq-vim

# Vanilla vi(m).
alias vi="vi -u NONE"

# Make Emacs startup only slow the first time you run it.
alias emacs="emacsclient -a '' -c"
alias remacs="killall emacs; command emacs --daemon"

# Make these more user-friendly.
alias which="type -a"
alias grep="grep --color=always"
alias amm="amm --no-remote-logging"
if command -v rlwrap >/dev/null; then
    alias ocaml="rlwrap ocaml"
fi
alias rg='rg --pretty'

# Replace these with something else.
if command -v nvim >/dev/null; then
    alias vim=nvim
fi
if command -v rg >/dev/null; then
    alias ack=rg
fi

# Switch between side-by-side and single-column delta diffs.
# Usage: e.g., type `2 git diff`.
alias 2='GIT_PAGER="delta -s"'

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
alias gds="g ds"
alias gh="g h"
alias gaa="g aa"
alias gpu="g pu"
alias gcl="g cl"
alias gco="g co"
alias gll="g ll"
alias gst="g st"
alias gsp="g sp"
alias gsl="g sl"
alias gb="g b"
alias gr="g r"
alias gcb="g cb"

# These are aliased to other things elsewhere.
#alias gc="g c"
#alias gd="g d"
#alias ga="g a"
#alias glo="g lo"

alias gdh="g d HEAD"
alias gdhh="g d HEAD^"
alias gdhhh="g d HEAD^^"
alias gdhhhh="g d HEAD^^^"
alias gdhhhhh="g d HEAD^^^^"
