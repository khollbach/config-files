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
# (Bells show up visually in tmux.)
bell() {
    time "$@"; echo -en '\a'
}



## Git shorthand functions

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



# Start tmux. If a session is already running that isn't attached to, then
# attach to that session instead of starting a new one.
ta() {
    local sessions
    local retval
    sessions=$(tmux ls 2> /dev/null)
    retval=$?

    # If tmux ls fails (ie no tmux server running), start a new session.
    if [[ $retval != 0 ]]; then
        tmux
        return
    fi

    local unattached_sessions
    unattached_sessions=$(echo "$sessions" | grep -v attached)

    # If there are no unattached sessions, start a new one.
    # Otherwise, attach to an existing one.
    if [[ "$unattached_sessions" == "" ]]; then
        tmux
    else
        local session
        session=$(echo "$unattached_sessions" | head -n 1 | cut -d ":" -f 1)
        tmux attach -t "$session"
    fi
}
