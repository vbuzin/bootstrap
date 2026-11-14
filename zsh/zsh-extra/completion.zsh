# completion.zsh — completion system configuration and compinit.
# Must be sourced after fpath is populated (brew shellenv in init.zsh).
# fzf-tab itself is loaded in init.zsh after this file.

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
zstyle ':completion:*:git-checkout:*' sort false

# -- Cache --
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"

# -- compinit --
autoload -Uz compinit
mkdir -p "$XDG_CACHE_HOME/zsh"
compinit -d "$XDG_CACHE_HOME/zsh/zcompdump"

# recognise the alias
compdef _editor=vi

# -- fzf-tab --
# Default preview for everything (cd, files…). Inlined — fzf-tab runs it in
# a subshell without access to zsh functions.
zstyle ':fzf-tab:complete:*' fzf-preview '
  if [[ -d $realpath ]]; then
    eza -T "$realpath" | head -200
  elif [[ -f $realpath ]]; then
    bat --style="numbers,changes" --color=always "$realpath"
  else
    echo "$realpath"
  fi
'
zstyle ':fzf-tab:complete:export:*' fzf-preview 'echo ${(P)word}'
zstyle ':fzf-tab:complete:unset:*'  fzf-preview 'echo ${(P)word}'
zstyle ':fzf-tab:complete:ssh:*'    fzf-preview 'dig +short $word 2>/dev/null || echo "no DNS info"'

zstyle ':fzf-tab:*' switch-group '<' '>'
zstyle ':fzf-tab:*' continuous-trigger ''   # disables auto-drill into subfolders
zstyle ':fzf-tab:*' use-fzf-default-opts yes   # colours from FZF_DEFAULT_OPTS
zstyle ':fzf-tab:*' fzf-flags --preview-window=right:65%
