# options.zsh — zsh setopt flags.
# History behaviour is configured here; the HIST* variables live in exports.zsh.
# History-substring-search arrow bindings live in init.zsh, next to the plugin.

bindkey -e   # emacs key bindings (Ctrl-A, Ctrl-E, Ctrl-K, Alt-F/B, etc.)

# -- Navigation keys the emacs keymap leaves unbound --
bindkey '^[[3~'    delete-char          # Delete
bindkey '^[[H'     beginning-of-line    # Home
bindkey '^[[F'     end-of-line          # End
bindkey '^[[1;3D'  backward-word        # Option+Left
bindkey '^[[1;3C'  forward-word         # Option+Right (also accepts one suggested word)
bindkey '^[[1;5D'  backward-word        # Ctrl+Left
bindkey '^[[1;5C'  forward-word         # Ctrl+Right

# Ctrl-X Ctrl-E — edit the current command line in $EDITOR
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# Space expands !!/!$ history references inline
bindkey ' ' magic-space

# Batch rename: zmv '(*).jpeg' '$1.jpg'
autoload -Uz zmv

setopt AUTO_CD                 # Type a directory name to cd into it
setopt AUTO_PUSHD              # cd pushes onto the dir stack; `cd -<Tab>` lists recent dirs
setopt PUSHD_IGNORE_DUPS       # No duplicate dirs on the stack
setopt PUSHD_SILENT            # Don't print the stack after every cd
setopt NO_BEEP                 # No bell on failed completion/search (also spares tmux [BELL])
setopt GLOB_COMPLETE           # Tab-complete globs rather than expanding them immediately
setopt INTERACTIVE_COMMENTS    # Allow `# comments` in pasted/typed commands

setopt HIST_IGNORE_ALL_DUPS    # Remove all earlier duplicates when a command is added
setopt HIST_SAVE_NO_DUPS       # Do not write duplicate entries to the history file
setopt HIST_IGNORE_SPACE       # space-prefixed commands are not recorded
setopt HIST_REDUCE_BLANKS      # Collapse superfluous whitespace before saving
setopt HIST_VERIFY             # Show `!!`/`!$` expansions for review before running
setopt SHARE_HISTORY           # Write immediately and share history between shells
setopt EXTENDED_HISTORY        # Record timestamp and elapsed time with each entry
setopt HIST_FCNTL_LOCK         # Use fcntl locking for safer concurrent history writes
