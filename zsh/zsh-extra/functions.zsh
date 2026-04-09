# functions.zsh — shell functions.

# -- Preferred editor: nvim if available, vi otherwise --
_editor() {
  if command -v nvim >/dev/null 2>&1; then
    nvim "$@"
  else
    command vi "$@"
  fi
}

_secret() {
  security find-generic-password -s "$1" -w 2>/dev/null
}

# -- Interactive Ripgrep (irg) --
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

    _editor "+call cursor($line, $col)" "$file"
}

# -- FZF completion runner --
# Overrides the default fzf completion preview for specific commands.
# Preview is inlined (not delegated to a shell function) because fzf
# runs --preview in a subshell with no access to zsh functions.
_fzf_comprun() {
  local command=$1
  shift

  local _preview='
  if [[ -d {} ]]; then
    eza -T {} | head -200
  elif [[ -f {} ]]; then
    bat --style="numbers,changes" --color=always {}
  else
    echo {}
  fi
  '

  case "$command" in
    cd)            fzf --preview "$_preview"              "$@" ;;
    export|unset)  fzf --preview "eval 'echo \$'{}"      "$@" ;;
    ssh)           fzf --preview 'dig +short {} 2>/dev/null || echo "no DNS info"' "$@" ;;
    *)             fzf --preview "$_preview"              "$@" ;;
  esac
}
