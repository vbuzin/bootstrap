# aliases.zsh — shell aliases.

# -- Navigation --
alias ll="eza -l --git"
alias la="eza -la --group-directories-first --git"

# -- Utilities --
alias cat="bat --paging=auto --wrap=never --style='changes'"
alias lg='XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}" lazygit'

# -- Editor
alias vi="_editor"
