# completion.zsh — completion system configuration and compinit.
# Must be sourced after fpath is populated (done in .zshrc before init.zsh).
# FZF completion widget is wired up in init.zsh after this file.

# -- Matching rules --
# 1. Case-insensitive match (a-z matches A-Za-z)
# 2. Partial-word match on separators (., -, _)
# 3. Substring match (anywhere in the candidate)
zstyle ':completion:*' matcher-list \
  'm:{a-z}={A-Za-z}' \
  'r:|[._-]=* r:|=*' \
  'l:|=* r:|=*'

zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu no   # Disable menu so fzf-tab can take over

# -- Cache --
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"

# -- compinit --
autoload -Uz compinit
mkdir -p "$XDG_CACHE_HOME/zsh"
compinit -d "$XDG_CACHE_HOME/zsh/zcompdump"

# AUTO_CD — type a directory name to cd into it without the cd command
setopt AUTO_CD
