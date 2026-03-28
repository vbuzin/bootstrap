# init.zsh — entry point for all interactive zsh config.
# Sourced from .zshrc after fpath is set up.
# Load order matters: exports → options → completion (compinit) →
# aliases → functions → fzf → autosuggestions → starship → direnv →
# syntax-highlighting (must be last).

# Homebrew environment — sets PATH, MANPATH, INFOPATH for non-login shells
[[ -x /opt/homebrew/bin/brew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"

ZSH_EXTRA_DIR="$HOME/.config/zsh-extra"

if [[ -d "$ZSH_EXTRA_DIR" ]]; then
  source "$ZSH_EXTRA_DIR/exports.zsh"      # Env vars, FZF opts, history paths
  source "$ZSH_EXTRA_DIR/options.zsh"      # setopt flags
  source "$ZSH_EXTRA_DIR/completion.zsh"   # zstyles + compinit
  source "$ZSH_EXTRA_DIR/aliases.zsh"
  source "$ZSH_EXTRA_DIR/functions.zsh"
fi

# FZF key bindings and completion widget (must come after compinit)
[[ -f "/opt/homebrew/opt/fzf/shell/completion.zsh" ]] && \
  source "/opt/homebrew/opt/fzf/shell/completion.zsh" 2>/dev/null
[[ -f "/opt/homebrew/opt/fzf/shell/key-bindings.zsh" ]] && \
  source "/opt/homebrew/opt/fzf/shell/key-bindings.zsh"

# zsh-autosuggestions
if [[ -f "/opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh" ]]; then
  source "/opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
  ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=245'
fi

# Starship prompt
if command -v starship >/dev/null 2>&1; then
  eval "$(starship init zsh)"
fi

# Direnv — per-directory env files (.envrc)
if command -v direnv >/dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi

# zsh-syntax-highlighting — must be sourced LAST in the init chain
[[ -f "/opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] && \
  source "/opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
