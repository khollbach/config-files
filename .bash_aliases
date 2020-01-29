#!/bin/bash

# update_configs script alias
alias upconf="source ~/config-files/update_configs"

# ls aliases
alias ls="ls -vh --color=auto"
alias la="ls -A"
alias ll="ls -lG"
alias lla="ll -lA"
alias lt="ll -tr"
alias lta="lt -A"

# `-` = `cs -`
alias -- -="cs -"

# `..` = `cs ..`, etc...
alias      ..="cs .."
alias     ...="cs ../.."
alias    ....="cs ../../.."
alias   .....="cs ../../../.."
alias  ......="cs ../../../../.."
alias .......="cs ../../../../../.."

# Save a lot of typing.
alias pag="ps aux | grep"
alias hig="history | cut -c 8- | uniq | grep"  # I mostly use C-r now.
alias f="fin"
alias fn="find . -name"
alias a="ack"
alias ai="ack -i"
alias t="bell"
alias p="python3"
alias v="vim"
alias l="less"
alias ltmk="latexmk -pdf -pvc"

# Make these more user-friendly.
alias which="type -a"
alias grep="grep --color=auto"
alias amm="amm --no-remote-logging"
if command -v rlwrap >/dev/null; then
    alias ocaml="rlwrap ocaml"
fi

# Misc.
alias u="unbuffer "
alias b="unbuffer -p "
alias sl="sl -e"
alias LS="LS -e"
alias rain="rain -d 150"

# ack -> rg
alias ack="ripgrep.rg"

# Neovim, if installed.
if command -v nvim >/dev/null; then
    alias vim="nvim"
fi

# Emacs
alias emacs="emacsclient -a '' -c"
alias remacs="killall emacs; command emacs --daemon"

# Git shorthands
alias gsh="git show"
alias gd="git diff"
alias gdh="gd HEAD"
alias gdhh="gd HEAD^"
alias gdhhh="gd HEAD^^"
alias gdhhhh="gd HEAD^^^"
alias gdhhhhh="gd HEAD^^^^"
alias ga="git add"
alias gaa="git add --all :/"
alias gb="git --no-pager branch"
alias gr="git remote"
alias gco="git checkout"
alias gl="git log"
alias glo="git log --oneline"
alias gst="git stash"
alias gsp="git stash pop"
alias gsl="git stash list"

# Recursive git shorthands
alias gs="gitrecurse git status"
alias gss="gs -sb"
alias gsi="gss --ignored"
alias gp="gitrecurse git pull --rebase"
alias gf="gitrecurse git fetch"
alias glh="gitrecurse git --no-pager log --oneline -n 10"
