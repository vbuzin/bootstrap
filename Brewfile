hostname = `hostname -s`.strip

is_tlnd = (hostname == "LT-Q5R6H9G174")
is_home = (hostname == "vbmaca")

brew "fzf"
brew "mas"
brew "p7zip"
brew "ripgrep"
brew "stow"
brew "tmux"

cask "1password"
cask "alacritty"
cask "brave-browser"
cask "docker"
cask "figma"
cask "firefox"
cask "microsoft-office" unless is_home
cask "notion"
cask "slack" unless is_home
cask "tidal"
cask "visual-studio-code"
cask "zoom" unless is_tlnd # managed

cask "homebrew/cask-drivers/logi-options-plus"
cask "homebrew/cask-fonts/font-cascadia-code"

mas "1Password for Safari", id: 1569813296
mas "AdGuard for Safari", id: 1440147259
mas "Affinity Designer", id: 824171161
mas "Affinity Photo", id: 824183456 unless is_tlnd 
mas "Affinity Publisher", id: 881418622 unless is_tlnd 

mas "Keynote", id: 409183694 unless is_tlnd
mas "Numbers", id: 409203825 unless is_tlnd
mas "Pages", id: 409201541 unless is_tlnd
