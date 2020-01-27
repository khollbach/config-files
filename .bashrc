#!/bin/bash

# This script sets the prompt, $PS1. It runs every time a prompt is printed.
function prompt_command {
    # Reset the terminal title after running any program, since it seems many
    # programs set it and then forget to clean it up when they exit.
    echo -ne "\e]0;\a"

    # Add a line break only if pwd is longer than 48 chars, or if
    # $prompt_contents is anything more than '\w'.
    local newline
    local w=$(dirs +0)
    if [ ${#w} -gt 48 ] || [ "$prompt_contents" != '\w' ]; then
        newline="\n"
    else
        newline=""
    fi

    local color='\[\e[1;38;5;'"$prompt_color"'m\]'
    local reset='\[\e[0m\]'
    PS1="$reset$color$prompt_contents$reset$newline"
    PS1="$PS1$color"'\$'"$reset "
}
PROMPT_COMMAND=prompt_command

#prompt_color=9  # Bright orange :)
prompt_color=5  # Pink!

# Concise prompt on my machines, verbose one elsewhere.
if [[ "$HOSTNAME" =~ kevan-|-lt ]]; then
    prompt_contents='\w'
else
    prompt_contents='\u@\h:\w'
fi



# Set LS_COLORS to not use any bold fonts.
eval `dircolors | sed s,01,00,g`

# Make other-writable directory names show as black text on a green background,
# instead of blue text against green (which is unreadable).
export LS_COLORS="${LS_COLORS}ow=30;42:"

# ripgrep config file
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# Git commit messages, etc.
export VISUAL=$(
    if command -v nvim >/dev/null; then
        command -v nvim
    else
        command -v vim
    fi)
export EDITOR=$VISUAL

# Unmap C-s from freezing tty output, so that it can be used for i-search.
stty -ixon



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



# Load aliases, functions.
source "$HOME/.bash_aliases"
source "$HOME/.bash_functions"

# Work-related defs, etc.
[ -f "$HOME/notes/work/bashrc" ] && source "$HOME/notes/work/bashrc"
