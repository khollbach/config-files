#!/bin/bash

# cd and ls
function cs {
    cd "$@" && ls
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

# Invoke rust compiler; pipe colorful stderr to less.
rc() {
    ub rustc "$@" 2>&1 | l
}

# https://gist.github.com/lukechilds/a83e1d7127b78fef38c2914c4ececc3c
get_latest_release() {
  curl --silent "https://api.github.com/repos/$1/releases/latest" | # Get latest release from GitHub api
    grep '"tag_name":' |                                            # Get tag line
    sed -E 's/.*"([^"]+)".*/\1/'                                    # Pluck JSON value
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
