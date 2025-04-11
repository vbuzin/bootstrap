PATH := $(PATH):/opt/homebrew/bin
SHELL := env PATH=$(PATH) /bin/bash

message  = " >>> ============ "$(1)" ============ <<< "

# all
# ==============================================================================
all: brew apps dotfiles tmux
.PHONY: all dotfiles apps brew alacritty _alacritty emacs _emacs nvim _nvim tmux

# dotfiles
# ==============================================================================
CSHELL := $(shell dscl . -read ~/ UserShell | sed 's/.*\/\(.*\)$$/\1/')
CONFIG_TARGET_DIR := $(abspath $(HOME)/.config)

dotfiles: $(CONFIG_TARGET_DIR)
	@echo $(call message,"Configuring prezto")
ifneq ($(CSHELL),zsh)
	@echo "Changing your shell to zsh"
	@chsh -s /bin/zsh
endif

ifeq (,$(wildcard $(HOME)/.zprezto))
	@echo $(call message,"Installing https://github.com/sorin-ionescu/prezto and submodules")
	@git clone --depth 1 --recursive https://github.com/sorin-ionescu/prezto.git $(HOME)/.zprezto
endif
	@stow --dotfiles --ignore=.DS_Store --override=.* --target=${HOME} dotfiles

$(CONFIG_TARGET_DIR):
	@mkdir -p $@

# apps
# ==============================================================================
apps: brew
	@echo $(call message,"Installing apps")
	@brew bundle --file Brewfile
	@stow --ignore=.DS_Store --override=.* --target=${HOME}/.config bat
	@bat cache --build

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

# brew
# ==============================================================================
brew:
ifneq (,$(shell which brew))
	@echo $(call message,"Updating Homebrew")
	@brew update && brew upgrade && brew upgrade --cask
else
	@echo $(call message,"Installing Homebrew")
	@/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
endif

# emacs
# ==============================================================================
emacs: brew
	@echo $(call message,"Installing and configuring Emacs")
	@brew install --cask emacs
	@brew install gnupg
	@stow --dotfiles --ignore=.DS_Store --override=.* --target=${HOME} emacs

_emacs: brew
	@echo $(call message,"Installing and configuring Emacs")
	@brew uninstall --zap emacs
	@brew uninstall --zap gnupg
	@brew autoremove
	@stow -D --dotfiles --ignore=.DS_Store --override=.* --target=${HOME} emacs

# firefox 
# ==============================================================================
firefox: brew
	@echo $(call message,"Installing and configuring Firefox")
	@brew install --cask firefox
	@FIREFOX_APP=/Applications/Firefox.app && \
		xattr -r -d com.apple.quarantine $$FIREFOX_APP && \
		stow --ignore=.DS_Store --override=.* -d firefox --target=$$FIREFOX_APP/Contents/Resources/ settings

firefox-cfg:
	@echo $(call message,"Provisioning userChrome and userContent")
	@FIREFOX_PROFILE=$$(grep -A1 '\[Install' "${HOME}/Library/Application Support/Firefox/profiles.ini" | grep 'Default=' | cut -d'/' -f2) && \
		FIREFOX_PROFILE_PATH="${HOME}/Library/Application Support/Firefox/Profiles/$$FIREFOX_PROFILE" && \
		stow --ignore=.DS_Store --override=.* -d firefox --target="$$FIREFOX_PROFILE_PATH" chrome

_firefox:
	@echo $(call message,"Uninstalling Firefox")
	@brew uninstall --zap --cask firefox
	@brew autoremove

# nvim
# ==============================================================================
nvim: brew dotfiles
	@echo $(call message,"Installing and configuring Nvim")
	@brew install neovim
	@stow --ignore=.DS_Store --override=.* --target=${HOME}/.config nvim

_nvim:
	@echo $(call message,"Unistalling Nvim and dependencies")
	@-brew uninstall neovim
	@brew autoremove
	@stow -D --ignore=.DS_Store --target=${HOME}/.config nvim
	@rm -rf ${HOME}/.local

# tmux
# ==============================================================================
tmux: brew dotfiles
	@echo $(call message,"Configuring tmux")
	@stow --ignore=.DS_Store --override=.* --target=${HOME}/.config tmux

