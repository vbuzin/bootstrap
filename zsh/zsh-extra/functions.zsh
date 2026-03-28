# functions.zsh — shell functions.

# -- Interactive Ripgrep (irg) --
# Two-phase search: start with ripgrep (Ctrl-R switches back),
# then narrow with fzf (Ctrl-F). Opens the selected file in nvim
# at the exact line and column.
irg() {
  RG_PREFIX="rg --hidden --column --line-number --no-heading --color=always --smart-case "
  INITIAL_QUERY="${*:-}"
  IFS=: read -rA selected <<<$(
    FZF_DEFAULT_COMMAND="$RG_PREFIX $(printf %q "$INITIAL_QUERY")" \
      fzf --ansi \
      --color "hl:-1:underline,hl+:-1:underline:reverse" \
      --disabled --query "$INITIAL_QUERY" \
      --bind "change:reload:sleep 0.1; $RG_PREFIX {q} || true" \
      --bind "ctrl-f:unbind(change,ctrl-f)+change-prompt(2. fzf> )+enable-search+clear-query+rebind(ctrl-r)" \
      --bind "ctrl-r:unbind(ctrl-r)+change-prompt(1. ripgrep> )+disable-search+reload($RG_PREFIX {q} || true)+rebind(change,ctrl-f)" \
      --bind "esc:abort" \
      --prompt '1. Ripgrep> ' \
      --delimiter : \
      --preview 'bat --style="numbers,changes" --color=always {1} --highlight-line {2}' \
      --preview-window 'up,70%,border-bottom,+{2}+3/3,~3'
  )

  [ -z "${selected[*]}" ] && return 0

  local file="${selected[1]}"
  local line="${selected[2]}"
  local col="${selected[3]}"

  nvim "+call cursor($line, $col)" "$file"
}

# -- Smart preview helper used by _fzf_comprun --
# Delegates to eza for directories, bat for files, falls back to echo.
__smart_preview() {
  local target="$1"

  if [[ -d "$target" ]]; then
    eza -T "$target" | head -200
  elif [[ -f "$target" ]]; then
    bat --style="numbers,changes" --color=always "$target"
  else
    echo "$target"
  fi
}

# -- FZF completion runner --
# Overrides the default fzf completion preview for specific commands.
_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)            fzf --preview '__smart_preview {}' "$@" ;;
    export|unset)  fzf --preview "eval 'echo \$'{}"    "$@" ;;
    ssh)           fzf --preview 'dig {}'              "$@" ;;
    *)             fzf --preview '__smart_preview {}' "$@" ;;
  esac
}
