#!/bin/bash

function l {
    cowthink -f turkey Gobble
}

# cd and ls
function cs {
    cd "$@" && ls
}

# Run evince in background and ignore stdout/stderr.
function ev {
    evince "$@" &> /dev/null &
}

# Run 'git commit', optionally accept a message for the '-m' option.
function gc {
    # Note that "$@" is special, and expands differently than "$foo".
    # That's why we declare "args" here.
    args=$@

    local msg_len=${#args}

    if [[ "$args" == "" ]]; then
        git commit
    elif [[ "$msg_len" -gt 50 ]]; then
        echo Commit message too long.
        echo Expected 50 chars, got "$msg_len".
        return 1
    else
        git commit -m "$args"
    fi
}

# gaa and gc.
function gca {
    gaa && gc "$@"
}

# gc and git push.
function gcp {
    gc "$@" && git push
}

# gca and git push.
function gcap {
    gca "$@" && git push
}

# Ignore hidden directories when ack'ing from $HOME.
function ack {
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
