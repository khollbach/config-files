#!/bin/bash

# cd and ls
function cs {
    cd "$@" && ls
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
function my_time {
    # When did I start?
    date
    echo

    # Run and preserve the return value.
    local rv
    time "$@"
    rv=$?

    # Ring a bell (which shows up visually in tmux), and trigger a system
    # notification (desktop popup).
    echo -en '\a'
    local command=$@  # Flatten arg array into string.
    if [[ "$rv" == 0 ]]; then
        notify-send "Command finished" "$command" -i dialog-information
    else
        notify-send "Command failed: error $rv" "$command" -i dialog-error
    fi

    return $rv
}



# Git shorthands:

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
