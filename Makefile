GRAY    := '\033[1;90m'
NC      := '\033[0m'

message  = $(GRAY)">>> ===== "$(1)" ====="$(NC)

CSHELL         := $(shell dscl . -read ~/ UserShell | sed 's/.*\/\(.*\)$$/\1/')

.PHONY: all brew apps dotfiles emacs

# all
# ==============================================================================
all: brew apps dotfiles

# dotfiles
# ==============================================================================
dotfiles:
	@echo $(call message,"Linking dotfiles")
ifneq ($(CSHELL),zsh)
	@echo "Changing your shell to zsh"
	@chsh -s /bin/zsh
endif

ifeq (,$(wildcard $(HOME)/.zprezto))
	@echo $(call message,"Installing https://github.com/sorin-ionescu/prezto and submodules")
	@git clone --depth 1 --recursive https://github.com/sorin-ionescu/prezto.git $(HOME)/.zprezto
endif
	@stow --dotfiles --ignore=.DS_Store --override=.* --target=${HOME} dotfiles

# apps
# ==============================================================================
apps: brew
	@echo $(call message,"Installing apps")
	@brew bundle --file Brewfile

# brew
# ==============================================================================
brew:
ifneq (,$(shell which brew))
	@echo $(call message,"Updating Homebrew")
	@brew update && brew upgrade && brew upgrade --cask
else
	@echo $(call message,"Installing Homebrew")
	@ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	@eval $(/opt/homebrew/bin/brew shellenv)
endif

# alacritty
# ==============================================================================
alacritty: brew dotfiles
	@echo $(call message,"Installing and configuring Alacritty")
	@brew install alacritty --cask
	@stow --ignore=.DS_Store --override=.* --target=${HOME}/.config alacritty

_alacritty:
	@echo $(call message,"Uninstalling alacritty")
	@-brew uninstall --cask alacritty
	@brew autoremove
	@stow -D --ignore=.DS_Store --target=${HOME}/.config alacritty

# emacs
# ==============================================================================
emacs: brew
	@echo $(call message,"Installing and configuring Emacs")
	@brew install emacs --cask
	@brew install aspell coreutils gnupg
	@stow --dotfiles --ignore=.DS_Store --override=.* --target=${HOME} emacs

# nvim
# ==============================================================================
nvim: brew dotfiles
	@echo $(call message,"Installing and configuring Nvim")
	@brew install neovim stylua
	@stow --ignore=.DS_Store --override=.* --target=${HOME}/.config nvim

_nvim:
	@echo $(call message,"Unistalling Nvim and dependencies")
	@-brew uninstall neovim stylua
	@brew autoremove
	@stow -D --ignore=.DS_Store --target=${HOME}/.config nvim
	@rm -rf ${HOME}/.local

