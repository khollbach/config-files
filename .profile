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
