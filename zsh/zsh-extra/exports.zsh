# exports.zsh — environment variables for interactive shells.
# Non-interactive / IDE env (1Password, PATH, DOTNET) lives in .zshrc Section 1.
# EDITOR, VISUAL, BROWSER, LANG live in .zprofile (available to login shells too).

# Word boundary characters — remove / so Alt+F/B navigates paths naturally
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# XDG Base Directory (explicit; many tools respect these)
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"

# Zsh history — store under XDG_STATE_HOME, not the home root
export HISTFILE="$XDG_STATE_HOME/zsh/history"
mkdir -p "${HISTFILE:h}"
export HISTSIZE=50000
export SAVEHIST=50000

# -- Tool configs --

export BAT_THEME="ansi"
export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git"

# -- FZF global styling (layout & colours only) --
export FZF_DEFAULT_OPTS='
--layout reverse
--border
--info=inline
--style full
--preview-window "up,60%"
--color=bg:#080909,fg:#abb2bf,hl:#4aa5f0
--color=bg+:#2c313a,fg+:#d7dae0,hl+:#4dc4ff
--color=info:#e5c07b,prompt:#61afef,pointer:#ff616e
--color=marker:#8cc265,spinner:#c162de,header:#7F838C
--color=border:#212121
'

# -- FZF History (Ctrl-R) — bat highlights the selected command as shell --
export FZF_CTRL_R_OPTS="
  --preview 'echo {} | bat --color=always --style=plain --language=sh'
  --preview-window down:3:wrap
"

# -- FZF File Search (Ctrl-T) — bat preview of file contents --
export FZF_CTRL_T_OPTS="
  --preview 'bat --color=always --style=numbers --line-range=:500 {}'
"

# -- FZF Directory Search (Alt-C) — eza tree preview --
export FZF_ALT_C_OPTS="
  --preview 'eza -T --color=always {} | head -200'
"
