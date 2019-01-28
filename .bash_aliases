#!/bin/bash

# update_configs script alias
alias upconf="source $HOME/config-files/update_configs"

# ls
alias ls="ls -vhG --color=auto"
alias ll="ls -l"
alias la="ls -A"
alias lla="ls -lA"
alias lt="ls -ltr"
alias lta="ls -ltrA"

# `-` = `cs -`
alias -- -="cs -"

# `..` = `cs ..`
# Etc.
alias      ..="cs .."
alias     ...="cs ../.."
alias    ....="cs ../../.."
alias   .....="cs ../../../.."
alias  ......="cs ../../../../.."
alias .......="cs ../../../../../.."



# Misc aliases
alias sl="sl -e"
alias rain="rain -d 150"
alias less="less -i -j4"
alias which="type -a"
alias ltmk="latexmk -pdf -pvc"
alias ta="tmux_attach"
alias fn="find . -name"
alias fin="find . -iname"

# Grep
alias grep="grep --color=auto"
alias pag="ps aux | grep"
alias hig="history | cut -c 8- | uniq | grep"

# ack -> rg
alias ack="ripgrep.rg"

# Neovim, if installed.
if command which nvim >/dev/null; then
    alias vim="nvim"
fi

# Emacs
alias emacs="emacsclient -a '' -c"
alias remacs="killall emacs; command emacs --daemon"



# Git shorthands
alias gd="git diff --color-words"
alias gdh="gd HEAD"
alias ga="git add"
alias gaa="git add --all :/"
alias gb="GIT_PAGER=less git branch"
alias gr="git remote"
alias gco="git checkout"
alias gl="git log"
alias glo="git log --oneline"

# Recursive git shorthands
alias gs="gitrecurse git status"
alias gss="gitrecurse git status -s"
alias gsi="gitrecurse git status -s --ignored"
alias gsb="gitrecurse git status -sb"
alias gp="gitrecurse git pull --rebase"
alias glh="gitrecurse git --no-pager log --oneline -n 10"
