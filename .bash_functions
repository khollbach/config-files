#!/bin/bash

# Nope.
l() {
    cowthink -f turkey Gobble
}

# cd and ls
cs() {
    cd "$@" && ls
}

# Run evince in background and ignore stdout/stderr.
ev() {
    evince "$@" &> /dev/null &
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
    sessions=$(tmux ls 2> /dev/null)
    retval=$?

    # If tmux ls fails (ie no tmux server running), start a new session.
    if [[ $retval != 0 ]]; then
        tmux
        return
    fi

    unattached_sessions=$(echo "$sessions" | grep -v attached)

    # If there are no unattached sessions, start a new one.
    # Otherwise, attach to an existing one.
    if [[ "$unattached_sessions" == "" ]]; then
        tmux
    else
        session=$(echo "$unattached_sessions" | head -n 1 | cut -d ":" -f 1)
        tmux attach -t "$session"
    fi
}



# Ignore hidden directories when ack'ing from $HOME.
ack() {
    if [[ `pwd` != ~ ]]; then
        command ack "$@"
        return
    fi

    # Get all hidden dirs, strip trailing slash, throw out '.' and '..'.
    local dirs=`ls -d .*/`
    dirs=`echo "$dirs" | sed 's!/$!!g'`
    dirs=`echo "$dirs" | grep -Ev '^.$|^..$'`

    # If there are single-quotes in a dir name, don't bother.
    if echo "$dirs" | grep -Eq "'"; then
        command ack "$@"
        return
    fi

    # Collect all these dirs in a list of options to pass to ack.
    # Wrap them in single quotes to handle special chars.
    local options=`echo "$dirs" | sed "s/^/--ignore-directory=is:'/g" | \
        sed "s/$/'/g"`

    # Run ack with these options.
    # Interpret the single quotes as syntax, and preserve word-boundaries in
    # the arguments passed to us from the command line.
    # To do all this, we must eval the options but *not* the arguments.

    local args=("$@")
    _pass_options_to_ack() {
        # Here "$@" are the options and "${args[@]}" are the arguments.
        command ack "$@" "${args[@]}"
    }

    # Options get eval'd, but arguments don't.
    eval _pass_options_to_ack $options

    # "Local" function.
    unset -f _pass_options_to_ack
}
