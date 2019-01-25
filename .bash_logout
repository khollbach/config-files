#!/bin/bash

# When leaving the console, clear the screen to increase privacy.
if [ "$SHLVL" = 1 ] && [ -x /usr/bin/clear_console ]; then
    /usr/bin/clear_console -q
fi
