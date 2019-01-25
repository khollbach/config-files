#!/bin/bash

l() {
    cowthink -f turkey Gobble
}

# cd and ls
cs() {
    cd "$@" && ls
}

# cd and lt
ct() {
    cd "$@" && lt
}



# Run evince in background and ignore stdout/stderr.
ev() {
    evince "$@" &> /dev/null &
}

# Time a long command and ring a bell when done.
# (Bells show up visually in tmux when a command in another window completes.)
bell() {
    # Preserve the return value.
    local rv
    time "$@"
    rv=$?

    echo -en '\a'
    return $rv
}



# Git shorthand functions

# Run 'git commit', optionally accept a message for the '-m' option.
gc() {
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

# gaa and gc
gca() {
    gaa && gc "$@"
}

# gc and git push
gcp() {
    gc "$@" && git push
}

# gca and git push
gcap() {
    gca "$@" && git push
}
