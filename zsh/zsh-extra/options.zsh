# options.zsh — zsh setopt flags.
# History behaviour is configured here; the HIST* variables live in exports.zsh.

bindkey -e   # emacs key bindings (Ctrl-A, Ctrl-E, Ctrl-K, Alt-F/B, etc.)

setopt GLOB_COMPLETE           # Tab-complete globs rather than expanding them immediately
setopt HIST_IGNORE_ALL_DUPS    # Remove all earlier duplicates when a command is added
setopt HIST_SAVE_NO_DUPS       # Do not write duplicate entries to the history file
setopt HIST_IGNORE_SPACE       # space-prefixed commands are not recorded
setopt INC_APPEND_HISTORY      # Write to the history file immediately, not on shell exit
setopt SHARE_HISTORY           # Share history between all active shells
setopt EXTENDED_HISTORY        # Record timestamp and elapsed time with each entry
setopt HIST_FCNTL_LOCK         # Use fcntl locking for safer concurrent history writes
