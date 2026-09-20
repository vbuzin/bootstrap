#!/bin/sh
# Wrapper: the real hook is notify-macos.py (JSON on stdin).
exec /usr/bin/python3 "$HOME/.grok/bin/notify-macos.py" "$@"
