hostname = `hostname -s`.strip

is_tlnd = (hostname == "LT-Q5R6H9G174")
is_home = (hostname == "vbmaca")

brew "bat"
brew "exa"
brew "fd"
brew "fzf"
brew "mas"
brew "p7zip"
brew "ripgrep"
brew "stow"
brew "tmux"

tap "homebrew/cask-versions"

cask "1password"
cask "affinity-designer"
cask "affinity-photo" unless is_tlnd
cask "affinity-publisher" unless is_tlnd
cask "brave-browser"
cask "docker"
cask "figma"
cask "firefox"
cask "iterm2"
cask "microsoft-office" unless is_home
cask "protonvpn" unless is_tlnd
cask "slack" unless is_home
cask "visual-studio-code"
cask "zoom" unless is_tlnd # managed

cask "homebrew/cask-drivers/logi-options-plus"
cask "homebrew/cask-fonts/font-cascadia-code"
cask "homebrew/cask-fonts/font-caskaydia-cove-nerd-font"

mas "1Password for Safari", id: 1569813296
mas "AdGuard for Safari", id: 1440147259
mas "Grammarly: Writing App", id: 1462114288

mas "Keynote", id: 409183694 unless is_tlnd
mas "Numbers", id: 409203825 unless is_tlnd
mas "Pages", id: 409201541 unless is_tlnd
