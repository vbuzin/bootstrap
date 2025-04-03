#!/bin/bash

EMACSCLIENT="/opt/homebrew/bin/emacsclient"

# Convert macOS-style paths (e.g., from "Open With") to POSIX
open_files=()
for f in "$@"; do
  open_files+=("$(realpath "$f")")
done

# If any frame exists, just raise Emacs
if $EMACSCLIENT -e '(> (length (frame-list)) 1)' 2>/dev/null | grep -q t; then
  osascript -e 'tell application "Emax" to activate'
  if [ ${#open_files[@]} -gt 0 ]; then
    "$EMACSCLIENT" -n "${open_files[@]}"
  fi
else
  # No frame — start a new GUI frame and open files
  "$EMACSCLIENT" -n -c -a "" "${open_files[@]}"
fi
