#!/bin/bash

# Quick hack to put the initial prompt at the bottom of the screen.
# Only run once per shell; i.e. not if I reload bashrc.
if [[ -z "$PROMPT_COMMAND" ]]; then
    # Print 100 newlines.
    for i in $(seq 1 100); do
        echo
    done
fi

# https://stackoverflow.com/questions/15883416/adding-git-branch-on-the-bash-command-prompt
parse_git_branch() {
    command git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

# This script sets the prompt, $PS1. It runs every time a prompt is printed.
function prompt_command {
    # Reset the terminal title after running any program, since it seems many
    # programs set it and then forget to clean it up when they exit.
    echo -ne "\e]0;\a"

    # Color escape codes.
    local color='\[\e[1;38;5;'"$prompt_color"'m\]'  # bold text, colorful
    local green='\[\e[38;5;2m\]'
    local reset='\[\e[0m\]'

    # Show the current git branch if you're in a git repo and you've checked
    # out anything other than master.
    local git_branch=$(parse_git_branch)
    if [ -n "$git_branch" ] \
        && [ "$git_branch" != '(master)' ] \
        && [ "$git_branch" != '(main)' ]
    then
        git_branch=" $green$git_branch$reset"
    else
        git_branch=""
    fi

    # Add a line break if $prompt_contents is anything more than '\w', or \w is
    # longer than 48 chars, or git branch is showing.
    local newline=""
    local w=$(dirs +0)
    if [[ -n "$prompt_contents" &&
        ("$prompt_contents" != '\w' || ${#w} -gt 48 || -n "$git_branch")
    ]]; then
        newline="\n"
    fi

    # Set the prompt; e.g.:
    # |
    # | ~/config-files (branch-name)
    # | $ _
    # or:
    # |
    # | ~/config-files$ _
    PS1="$reset\n"
    PS1="$PS1$color$prompt_contents$reset$git_branch$newline"
    PS1="$PS1$color"'\$'"$reset "
}
PROMPT_COMMAND=prompt_command

# Concise prompt on my machines, verbose one elsewhere.
if [[ "$HOSTNAME" =~ kevan-|-dt ]]; then
    prompt_contents='\w'
    prompt_color=5  # Pink!
elif [[ "$HOSTNAME" =~ -lt ]]; then
    prompt_contents='\w'
    prompt_color=6  # Cyan
else
    prompt_contents='\u@\h:\w'
    prompt_color=9  # Bright orange :)
fi



# Set LS_COLORS to not use any bold fonts.
eval `dircolors | sed s,01,00,g`

# Make other-writable directory names show as black text on a green background,
# instead of blue text against green (which is unreadable).
export LS_COLORS="${LS_COLORS}ow=30;42:"

# Unmap C-s from freezing tty output, so that it can be used for i-search.
stty -ixon

# Filter duplicates out of history.
export HISTCONTROL=ignoredups

# Keep a much longer history file (default is 500 lines).
export HISTSIZE=100000

# Don't leave .pyc files or __pycache__ dirs lying around.
export PYTHONDONTWRITEBYTECODE=1

# ripgrep config file location.
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# Change bat colorscheme; remove line numbers; always use pager.
# See https://github.com/sharkdp/bat
export BAT_THEME='Solarized (dark)'
export BAT_STYLE=plain
export BAT_PAGER='less --'



# fzf colorscheme.
export FZF_DEFAULT_OPTS="--color dark"

# Show hidden files in fzf, respecting ripgrep's ignores (.git, etc).
export FZF_DEFAULT_COMMAND='rg --hidden -l ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# Show hidden directories in fzf's change directory keybind.
export FZF_ALT_C_COMMAND='
    if [[ "$PWD" != "$HOME" ]]; then
        fd --type d --hidden --exclude .git
    else
        # Ignore hidden dirs and ~/snap when used from home directory.
        fd --type d --exclude .git --exclude /snap
    fi
'

# Use alt-u for fzf cd.
bind -m emacs-standard '"\eu": " \C-b\C-k \C-u`__fzf_cd__`\e\C-e\er\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d"'

# Add keybinds to fzf forgit.
export FORGIT_FZF_DEFAULT_OPTS='
    --height=100%
    --bind=n:down,e:up
    --bind=a:abort
'

# forgit: use `o` instead of `enter` to preview selection.
# Should run only once.
if [ -d ~/.opt/forgit ] && grep -q -E 'enter:execute' ~/.opt/forgit/forgit.plugin.sh; then
    echo forgit: remap o to enter
    sed -i -E 's,enter:execute,o:execute,g' ~/.opt/forgit/forgit.plugin.sh
fi



# Use bat for better syntax highlighting of man pages.
# See https://github.com/sharkdp/bat
if command -v bat >/dev/null; then
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"
    export MANROFFOPT=-c
fi

# Set the terminal title when opening man pages.
function man {
    title_wrapper man "$@"
}

# Run a command, but set the terminal title to the first non-flag argument.
function title_wrapper {
    local cmd=$1
    shift
    local args=("$@")

    local title
    while [[ -n "$1" ]]; do
        # If the arg starts with anything but `-`, use it as the title.
        # This isn't perfect, but it's good enough.
        if [[ "$1" =~ ^[^-] ]]; then
            title=$1
            break
        fi

        shift
    done

    # Set the terminal title
    if [[ -n "$title" ]]; then
        echo -ne "\e]0;$title\a"
    fi

    command "$cmd" "${args[@]}"
}



# Load fzf shell keybinds.
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Load fzf forgit aliases.
[ -f ~/.opt/forgit/forgit.plugin.sh ] && source ~/.opt/forgit/forgit.plugin.sh

# Load aliases, functions.
source ~/.bash_aliases
source ~/.bash_functions

# Work-related defs, etc.
[ -f ~/notes/.configs/bashrc ] && source ~/notes/.configs/bashrc
