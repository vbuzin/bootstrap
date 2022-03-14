GRAY    := '\033[1;90m'
NC      := '\033[0m'

message  = $(GRAY)">>> ===== "$(1)" ====="$(NC)

all: _brew _apps _git _emacs _rust _haskell _zsh _alacritty
.PHONY: all list $(MAKECMDGOALS)

# Installing utils and apps via Homebrew
# ==============================================================================
APPS_SOURCE_DIR      := $(abspath ./apps)

GPG_SOURCE_DIR       := $(abspath ./gnupg)
GPG_TARGET_DIR       := $(abspath $(HOME)/.gnupg)

HTOP_SOURCE_DIR      := $(abspath ./htop)
HTOP_TARGET_DIR      := $(abspath $(HOME)/.config/htop/)

TMUX_SOURCE_DIR      := $(abspath ./tmux)
TMUX_TARGET_DIR      := $(abspath $(HOME)/.config/tmux/)

_apps: _brew | $(GPG_TARGET_DIR) $(HTOP_TARGET_DIR) $(TMUX_TARGET_DIR)
	@echo $(call message,"Installing utils and apps via Homebrew")
	@brew bundle --file=$(APPS_SOURCE_DIR)/Brewfile

	@echo $(call message,"Setting up configuration files for GnuPG")
	ln -sf $(GPG_SOURCE_DIR)/gpg.conf       $(GPG_TARGET_DIR)
	ln -sf $(GPG_SOURCE_DIR)/gpg-agent.conf $(GPG_TARGET_DIR)

	@echo $(call message,"Setting up configuration files for htop")
	cp -f $(HTOP_SOURCE_DIR)/htoprc $(HTOP_TARGET_DIR)

	@echo $(call message,"Setting up configuration files for tmux")
	ln -sf $(TMUX_SOURCE_DIR)/tmux.conf $(TMUX_TARGET_DIR)

$(GPG_TARGET_DIR):
	@echo "Creating directory $(GPG_TARGET_DIR)"
	@mkdir -p $@
	@chmod 700 $(GPG_TARGET_DIR)

$(HTOP_TARGET_DIR):
	@echo "Creating directory $(HTOP_TARGET_DIR)"
	@mkdir -p $@

$(TMUX_TARGET_DIR):
	@echo "Creating directory $(TMUX_TARGET_DIR)"
	@mkdir -p $@

# Alacritty
# ==============================================================================
ALACRITTY_SOURCE_DIR := $(abspath ./alacritty)
ALACRITTY_TARGET_DIR := $(abspath $(HOME)/.config/alacritty/)

_alacritty: | $(ALACRITTY_TARGET_DIR)
	@echo $(call message,"Installing utils and apps via Homebrew")
	@brew bundle --file=$(ALACRITTY_SOURCE_DIR)/Brewfile

	@echo $(call message,"Setting up configuration files for alacritty")
	ln -sf $(ALACRITTY_SOURCE_DIR)/alacritty.yml  $(ALACRITTY_TARGET_DIR)

$(ALACRITTY_TARGET_DIR):
	@echo "Creating directory $(ALACRITTY_TARGET_DIR)"
	@mkdir -p $@

# installing Homebrew if not installed, otherwise updating
# ==============================================================================
_brew:
ifneq (,$(shell which brew))
	@echo $(call message,"Updating Homebrew")
	@brew update && brew upgrade && brew upgrade --cask
else
	@echo $(call message,"Installing Homebrew")
	@ruby -e "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
endif

# Emacs
# ==============================================================================
EMACS_SOURCE_DIR   := $(abspath ./emacs)
EMACS_TARGET_DIR   := $(abspath $(HOME)/.emacs.d)
ESHELL_TARGET_DIR  := $(EMACS_TARGET_DIR)/eshell
EMACS_CONFIG_FILES := $(wildcard $(EMACS_SOURCE_DIR)/*.el)

_emacs: _brew | $(EMACS_TARGET_DIR) $(ESHELL_TARGET_DIR)
	@echo $(call message,"Installing required packages via Homebrew")
	@brew bundle --file=$(EMACS_SOURCE_DIR)/Brewfile

	@echo $(call message,"Setting up configuration files for emacs")
	ln -sf $(EMACS_CONFIG_FILES)        $(EMACS_TARGET_DIR)
	ln -sf $(EMACS_SOURCE_DIR)/snippets $(EMACS_TARGET_DIR)
	ln -sf $(EMACS_SOURCE_DIR)/alias    $(ESHELL_TARGET_DIR)/alias

$(EMACS_TARGET_DIR):
	@echo "Creating directory $(EMACS_TARGET_DIR)"
	@mkdir -p $@

$(ESHELL_TARGET_DIR):
	@echo "Creating directory $(ESHELL_TARGET_DIR)"
	@mkdir -p $@

# Git
# ==============================================================================
GIT_SOURCE_DIR := $(abspath ./git)
GIT_TARGET_DIR := $(HOME)
HOSTNAME       := $(shell hostname)

_git:
	@echo $(call message,"Configuring git")
ifeq ($(HOSTNAME),vbmaca.local)
	ln -sf $(GIT_SOURCE_DIR)/gitconfig.pers $(GIT_TARGET_DIR)/.gitconfig
endif
	ln -sf $(GIT_SOURCE_DIR)/gitignore $(GIT_TARGET_DIR)/.gitignore

# Haskell
# ==============================================================================
HASKELL_SOURCE_DIR := $(abspath ./haskell)
GHC_TARGET_DIR     := $(abspath $(HOME)/.ghc)
STACK_TARGET_DIR   := $(abspath $(HOME)/.stack)

_haskell: _brew | $(GHC_TARGET_DIR) $(STACK_TARGET_DIR)
	@echo $(call message,"Installing/configuring haskell stack and utils")
	@brew install haskell-stack
	@stack setup #install the GHC compiler
	@stack install hindent hlint hoogle
	ln -sf $(HASKELL_SOURCE_DIR)/ghci.conf   $(GHC_TARGET_DIR)
	ln -sf $(HASKELL_SOURCE_DIR)/config.yaml $(STACK_TARGET_DIR)

$(GHC_TARGET_DIR):
	@echo "Creating directory $(GHC_TARGET_DIR)"
	@mkdir -p $@

$(STACK_TARGET_DIR):
	@echo "Creating directory $(STACK_TARGET_DIR)"
	@mkdir -p $@

# Mu for brave and true
# ==============================================================================
MU_SOURCE_DIR := $(abspath ./mu)

MAIL_TARGET_DIR1 := $(abspath $(HOME)/Mail/v8v.buzin@gmail.com)
MAIL_TARGET_DIR2 := $(abspath $(HOME)/Mail/v.buzin@icloud.com)

_mu: _emacs | $(MAIL_TARGET_DIR1) $(MAIL_TARGET_DIR2)
	@echo $(call message,"Installing isync and mu")
	@brew install mu isync

	ln -sf $(MU_SOURCE_DIR)/mbsyncrc $(HOME)/.mbsyncrc

	@echo "Synchronising mailbox(es) and indexing via mu"
	@mbsync --all --verbose
	@mu index --maildir=$(MAIL_TARGET_DIR1)
	@mu index --maildir=$(MAIL_TARGET_DIR2)

$(MAIL_TARGET_DIR1):
	@echo "Creating directory $(MAIL_TARGET_DIR1)"
	@mkdir -p $@

$(MAIL_TARGET_DIR2):
	@echo "Creating directory $(MAIL_TARGET_DIR2)"
	@mkdir -p $@

# Rust
# ==============================================================================
RUSTUP_COMPL_TARGET_DIR := $(abspath $(HOME)/.config/rust/functions)

_rust: _brew _zsh | $(RUSTUP_COMPL_TARGET_DIR)
	@echo $(call message,"Installing/configuring rust and utils")
	@brew install rustup-init
	@brew install rust-analyzer
	@rustup-init -y --no-modify-path --default-toolchain stable
	@rustup toolchain add nightly
	@rustup component add rust-src
	@rustup completions zsh > $(RUSTUP_COMPL_TARGET_DIR)/_rustup

$(RUSTUP_COMPL_TARGET_DIR):
	@echo "Creating directory $(RUSTUP_COMPL_TARGET_DIR)"
	@mkdir -p $@

# zsh
# ==============================================================================
CSHELL         := $(shell dscl . -read ~/ UserShell | sed 's/.*\/\(.*\)$$/\1/')
ZSH_SOURCE_DIR := $(abspath ./zsh)
ZSH_TARGET_DIR := $(HOME)
ZPREZTO_DIR    := $(ZSH_SOURCE_DIR)

_zsh:
	@echo $(call message,"Configuring zsh")
ifneq ($(CSHELL),zsh)
	@echo "Changing your shell to zsh"
	@chsh -s /bin/zsh
endif

	@echo $(call message,"Updating https://github.com/sorin-ionescu/prezto and submodules")
	git submodule update --init --remote --recursive --force zsh/zprezto

	@echo $(call message,"Setting up configuration files for zsh")
	ln -sf $(ZSH_SOURCE_DIR)/zprezto   $(ZSH_TARGET_DIR)/.zprezto
	ln -sf $(ZSH_SOURCE_DIR)/zprofile  $(ZSH_TARGET_DIR)/.zprofile
	ln -sf $(ZSH_SOURCE_DIR)/zshrc     $(ZSH_TARGET_DIR)/.zshrc
	ln -sf $(ZSH_SOURCE_DIR)/zpreztorc $(ZSH_TARGET_DIR)/.zpreztorc

# vim
# ==============================================================================
VIM_SOURCE_DIR := $(abspath ./vim)
VIM_TARGET_DIR := $(HOME)

_vim:
	@echo $(call message,"Installing plugins")
	git submodule update --init --remote --recursive --force vim/vim

	@echo $(call message,"Setting up configuation for Vim")
	ln -sf $(VIM_SOURCE_DIR)/vimrc $(VIM_TARGET_DIR)/.vimrc
	ln -sf $(VIM_SOURCE_DIR)/vim   $(VIM_TARGET_DIR)/.vim
