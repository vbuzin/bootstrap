# init.zsh — entry point for all interactive zsh config.
# Sourced from .zshrc.
# Load order matters: exports → options → functions → completion (compinit) →
# aliases → fzf → fzf-tab → autosuggestions → starship → direnv →
# syntax-highlighting → history-substring-search (must follow highlighting).

# Homebrew environment — PATH, fpath (site-functions, before compinit),
# MANPATH, INFOPATH, HOMEBREW_PREFIX. Use an
# absolute brew path so this still works when the parent process (GUI
# terminal, Zellij) has a bare PATH.
if [[ -x /opt/homebrew/bin/brew ]]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [[ -x /usr/local/bin/brew ]]; then
  eval "$(/usr/local/bin/brew shellenv)"
fi
# shellenv prepends via the PATH string, which bypasses `typeset -U path`.
path=($path)
_brew_share="${HOMEBREW_PREFIX:-/opt/homebrew}/share"

ZSH_EXTRA_DIR="$HOME/.config/zsh-extra"

if [[ -d "$ZSH_EXTRA_DIR" ]]; then
  source "$ZSH_EXTRA_DIR/exports.zsh"      # Env vars, FZF opts, history paths
  source "$ZSH_EXTRA_DIR/options.zsh"      # setopt flags + key bindings
  source "$ZSH_EXTRA_DIR/functions.zsh"
  source "$ZSH_EXTRA_DIR/completion.zsh"   # zstyles + compinit
  source "$ZSH_EXTRA_DIR/aliases.zsh"
fi

# FZF key bindings (Ctrl-R, Ctrl-T, Alt-C) and ** completion
(( $+commands[fzf] )) && source <(fzf --zsh)

# fzf-tab — after compinit, before plugins that wrap widgets
[[ -f "$_brew_share/fzf-tab/fzf-tab.zsh" ]] && source "$_brew_share/fzf-tab/fzf-tab.zsh"

# zsh-autosuggestions
if [[ -f "$_brew_share/zsh-autosuggestions/zsh-autosuggestions.zsh" ]]; then
  source "$_brew_share/zsh-autosuggestions/zsh-autosuggestions.zsh"
  ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=245'
  ZSH_AUTOSUGGEST_STRATEGY=(history completion)
  ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=40   # skip suggestions for big pastes
  bindkey '^ ' autosuggest-accept      # Ctrl-Space accepts the whole suggestion
fi

# Starship prompt
(( $+commands[starship] )) && eval "$(starship init zsh)"

# OSC 133 prompt marks — lets tmux copy-mode jump between prompts ([ / ]).
# A = prompt start, C = command output start.
_osc133_prompt()  { print -n '\e]133;A\a' }
_osc133_command() { print -n '\e]133;C\a' }
autoload -Uz add-zsh-hook
add-zsh-hook precmd  _osc133_prompt
add-zsh-hook preexec _osc133_command

# Direnv — per-directory env files (.envrc)
(( $+commands[direnv] )) && eval "$(direnv hook zsh)"

# zsh-syntax-highlighting — after every widget-defining plugin
[[ -f "$_brew_share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] && \
  source "$_brew_share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# zsh-history-substring-search — its README requires loading after
# syntax-highlighting. Arrow keys: normal (^[[A) and application (^[OA) mode.
if [[ -f "$_brew_share/zsh-history-substring-search/zsh-history-substring-search.zsh" ]]; then
  source "$_brew_share/zsh-history-substring-search/zsh-history-substring-search.zsh"
  bindkey '^[[A' history-substring-search-up
  bindkey '^[OA' history-substring-search-up
  bindkey '^[[B' history-substring-search-down
  bindkey '^[OB' history-substring-search-down
fi

unset _brew_share
