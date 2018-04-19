# Run setxkbmap only if X is running and we're not inside tmux.
if [ -n "$DISPLAY" ] && [ -z "$TMUX" ]; then
    setxkbmap -option ctrl:nocaps
fi

# Source .bashrc if running bash
# Skip this on CDF; .bashrc sources .profile on cdf,
# since cdf ignores .profile, and we don't want an infinite loop.
if [ -n "$BASH_VERSION" ] && [ "$USER" != hollbac1 ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# Include private bin dirs in path
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/bin:$PATH

# Include scripts dir in path
export PATH=$HOME/config-files/scripts:$PATH

# Include rust binaries in path
export PATH=$HOME/.cargo/bin:$PATH
