#!/bin/bash

# cd and ls
function _cs {
    cd "$@" && ls
}

# Case-insensitive `find` alias
function my_find {
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
        command git commit
    elif [[ "$msg_len" -gt 50 ]]; then
        echo Commit message too long.
        echo Expected 50 chars, got "$msg_len".
        return 1
    else
        command git commit -m "$message"
    fi
}

# "commit all"
function gca {
    command git add --all :/ && gc "$@"
}

# "commit push"
function gcp {
    gc "$@" && command git push
}

# "commit all push"
function gcap {
    gca "$@" && command git push
}
