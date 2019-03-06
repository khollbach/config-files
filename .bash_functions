#!/bin/bash

# cd and ls
function cs {
    cd "$@" && ls
}

# cd and lt
function ct {
    cd "$@" && lt
}

# Case-insensitive `find` alias
function fin {
    find . -iname "*$1*" "${@:2}"
}

# Run evince in background and ignore stdout/stderr.
function ev {
    evince "$@" &> /dev/null &
}

# Time a long command and ring a bell when done.
# (Bells show up visually in tmux when a command in another window completes.)
function bell {
    date
    echo

    # Preserve the return value.
    local rv
    time "$@"
    rv=$?

    echo -en '\a'
    return $rv
}

# Git shorthands

# Run 'git commit', optionally accept a message for the '-m' option.
function gc {
    # Flatten arguments into a single string
    local message
    message=$@

    local msg_len
    msg_len=${#message}

    if [[ "$msg_len" -eq 0 ]]; then
        git commit
    elif [[ "$msg_len" -gt 50 ]]; then
        echo Commit message too long.
        echo Expected 50 chars, got "$msg_len".
        return 1
    else
        git commit -m "$message"
    fi
}

# add all and commit
function gca {
    gaa && gc "$@"
}

# commit and push
function gcp {
    gc "$@" && git push
}

# add all, commit, push
function gcap {
    gca "$@" && git push
}
