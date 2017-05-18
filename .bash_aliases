#!/bin/bash

# Update configs alias
alias upconf="source $HOME/config-files/update_configs"

# Misc aliases
alias ls="ls -vh --color=auto"
alias ll="ls -l"
alias la="ls -A"
alias lla="ls -lA"
alias lt="ls -ltr"
alias lta="ls -ltrA"
alias grep="grep --color=auto"
alias pag="ps aux | grep"
alias hig="history | grep"
alias ltmk="latexmk -pdf -pvc"

# Git shorthands
alias gd="git diff --color-words"
alias ga="git add"
alias gaa="git add --all :/"
alias gb="git branch"
alias gr="git remote"
alias gco="git checkout"
alias gl="git log"
alias glo="git log --oneline"

# Recursive git shorthands
alias gdr="gitrecurse git diff --color-words"
alias gs="gitrecurse git status"
alias gss="gitrecurse git status -s"
alias gsi="gitrecurse git status -s --ignored"
alias gsb="gitrecurse git status -sb"
alias gp="gitrecurse git pull"
alias glh="gitrecurse git --no-pager log --oneline -n 10"
