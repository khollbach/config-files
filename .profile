# Map capslock to control
setxkbmap -option ctrl:nocaps

# Source .bashrc if running bash
if [ -n "$BASH_VERSION" ]; then
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# Include private bin directories in PATH
PATH="$HOME/bin:$HOME/.local/bin:$PATH"
