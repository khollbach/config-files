#!/bin/sh

# Add directories containing executables to `PATH`.
export PATH=~/.config-files/scripts:$PATH
export PATH=~/.local/bin:$PATH
export PATH=~/bin:$PATH
export PATH=~/.cargo/bin:$PATH
export PATH=/usr/local/go/bin:$PATH
if command -v go >/dev/null; then
    # Add ~/go/bin to PATH.
    export PATH=$(go env GOPATH)/bin:$PATH
fi

# Set editor for git commit messages, etc.
export VISUAL=$(command -v nvim || command -v vim || command -v vi)
export EDITOR=$VISUAL

if [[ -f ~/.personal-configs/profile ]]; then
    source ~/.personal-configs/profile
fi
