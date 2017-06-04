# Run setxkbmap only if X is running and we're not inside tmux.
if [ -n "$DISPLAY" ] && [ -z "$TMUX" ]; then
    setxkbmap -option ctrl:nocaps
fi

# Source .bashrc if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# Include scripts dir in path
export PATH=$HOME/config-files/scripts:$PATH

# Include private bin dirs in path
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/bin:$PATH
