if [ -x /opt/homebrew/bin/brew ]; then
  eval $(/opt/homebrew/bin/brew shellenv)
fi

# Completion paths
if type brew &>/dev/null; then
    fpath+=($(brew --prefix)/share/zsh/site-functions)
fi