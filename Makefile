GRAY    := '\033[1;90m'
NC      := '\033[0m'

message  = $(GRAY)">>> ===== "$(1)" ====="$(NC)

CSHELL         := $(shell dscl . -read ~/ UserShell | sed 's/.*\/\(.*\)$$/\1/')
ZSH_SOURCE_DIR := $(abspath ./zsh)
ZSH_TARGET_DIR := $(HOME)
ZPREZTO_DIR    := $(ZSH_SOURCE_DIR)

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
	@stow dotfiles

# apps
# ==============================================================================
apps: brew
	@echo $(call message,"Installing apps")
	@brew bundle --file Brewfile

# emacs
# ==============================================================================
emacs: brew
	@echo $(call message,"Installing and configuring Emacs")
	@brew install emacs --cask
	@brew install aspell coreutils gnupg
	@stow emacs

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