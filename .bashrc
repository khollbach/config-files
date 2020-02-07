#!/bin/bash

# https://stackoverflow.com/questions/15883416/adding-git-branch-on-the-bash-command-prompt
parse_git_branch() {
    command git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

# This script sets the prompt, $PS1. It runs every time a prompt is printed.
function prompt_command {
    # Reset the terminal title after running any program, since it seems many
    # programs set it and then forget to clean it up when they exit.
    echo -ne "\e]0;\a"

    local color='\[\e[1;38;5;'"$prompt_color"'m\]'  # bold, colorful
    local green='\[\e[38;5;2m\]'  # green
    local reset='\[\e[0m\]'

    # Add a line break only if pwd is longer than 48 chars, or if
    # $prompt_contents is anything more than '\w'.
    local newline=""
    local w=$(dirs +0)
    if [ ${#w} -gt 48 ] || [ "$prompt_contents" != '\w' ]; then
        newline="\n"
    fi

    # Show the current git branch if you're in a git repo and you've checked
    # out anything other than master.
    local git_branch=$(parse_git_branch)
    if [ -n "$git_branch" ] && [ "$git_branch" != '(master)' ]; then
        git_branch=" $green$git_branch$reset"
        newline="\n"
    else
        git_branch=""
    fi

    # Set the prompt.
    PS1="$reset$color$prompt_contents$reset$git_branch$newline"
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

# ripgrep config file
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# Unmap C-s from freezing tty output, so that it can be used for i-search.
stty -ixon

# Filter duplicates out of history.
# Also, if you precede a command with a space, it won't appear in history.
# This is good for passwords on the commandline (which shouldn't happen).
export HISTCONTROL=ignoreboth

# Don't leave .pyc files or __pycache__ dirs lying around.
export PYTHONDONTWRITEBYTECODE=1



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



# Get fzf to work.
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Load aliases, functions.
source "$HOME/.bash_aliases"
source "$HOME/.bash_functions"

# Work-related defs, etc.
[ -f "$HOME/notes/work/bashrc" ] && source "$HOME/notes/work/bashrc"
