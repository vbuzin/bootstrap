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

# recognise the alias
compdef _editor=vi

# AUTO_CD — type a directory name to cd into it without the cd command
setopt AUTO_CD

# fzf-tab configuration — smart preview inlined + continuous completion disabled
# disable sort when completing `git checkout`
zstyle ':completion:*:git-checkout:*' sort false

# force zsh not to show completion menu
zstyle ':completion:*' menu no

# Smart preview (inlined — works reliably in fzf-tab's subshell)
zstyle ':fzf-tab:complete:cd:*' fzf-preview '
  if [[ -d $realpath ]]; then
    eza -T "$realpath" | head -200
  elif [[ -f $realpath ]]; then
    bat --style="numbers,changes" --color=always "$realpath"
  else
    echo "$realpath"
  fi
'
zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview '
  if [[ -d $realpath ]]; then
    eza -T "$realpath" | head -200
  elif [[ -f $realpath ]]; then
    bat --style="numbers,changes" --color=always "$realpath"
  else
    echo "$realpath"
  fi
'
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

# === CRITICAL FIXES ===
zstyle ':fzf-tab:*' switch-group '<' '>'
zstyle ':fzf-tab:*' continuous-trigger ''          # ← disables auto-drill into subfolders
zstyle ':fzf-tab:*' fzf-flags --color=bg:#080909,fg:#abb2bf,hl:#4aa5f0 \
                                 --color=bg+:#2c313a,fg+:#d7dae0,hl+:#4dc4ff \
                                 --preview-window=right:65%
