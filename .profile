#!/bin/sh

# Don't give others permissions to my files.
umask 077

# Include private bin dirs in path
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/bin:$PATH

# Include scripts dir in path
export PATH=$HOME/config-files/scripts:$PATH

# Include rust binaries in path
export PATH=$HOME/.cargo/bin:$PATH

# setxkbmap will only work if X is running.
if [ -n "$DISPLAY" ]; then
    # Map capslock to control
    setxkbmap -option ctrl:nocaps
fi

# Start redshift if it isn't already running.
# Currently I have the issue that it doesn't terminate when I log out.
# And thus when I log back in after, the screen won't be red-shifted, because
# the old instance is still running but isn't affecting the current session.
# (Manual fix is to `killall redshift; redshift &`)
if [ -n "$DISPLAY" ] && command which redshift &>/dev/null \
    && [ -z "`ps aux | grep redshift | grep -v grep`" ]; then

    redshift &> /dev/null &
fi

# Launch an Emacs daemon for faster load times.
# Emacs can then be started as `emacsclient -c`.
if command which emacs > /dev/null \
    && [ -z "`ps aux | grep emacs | grep -v grep`" ]; then

    # (TODO) Disabled while I figure out my emacs configs.
    #command emacs --daemon &> /dev/null
    :
fi
