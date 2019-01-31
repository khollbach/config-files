#!/bin/bash

# cd and ls
function cs {
    cd "$@" && ls
}

# cd and lt
function ct {
    cd "$@" && lt
}

# Pager: Make a program think it's writing to a tty (to get colored output,
# etc), but redirect the output to less.
# Requires the package `expect` installed for the `unbuffer` command.
# If you want to pass arguments to less this way, you can do, e.g.:
#   LESS=-S l ack somestring
#   LESS='-F -X' l ack somestring
# TODO: the above actually doesn't work when LESS is already defined in a
#       .lesskey #env ... :(  Fix / workaround = ???
function l {
    unbuffer "$@" | less
}

# Allow alias expansion within its arguments.
alias l="l "

# Temporary workaround for the issue of `LESS=asdf less ...` not working when
# .lesskey already specifies a value for LESS.
function l-S {
    unbuffer "$@" | less -S
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
    # Preserve the return value.
    local rv
    time "$@"
    rv=$?

    echo -en '\a'
    return $rv
}

# Git shorthand functions

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
