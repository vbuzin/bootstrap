# Variables
PATH             := $(PATH):/opt/homebrew/bin
SHELL            := env PATH=$(PATH) /bin/bash
CONFIG_DIR       := $(HOME)/.config
EMACS_CONFIG_DIR := $(HOME)/.emacs.d
PREZTO_DIR       := $(HOME)/.zprezto
BREWFILE         := $(CURDIR)/Brewfile
STOW_OPTS        := --ignore=.DS_Store --override=.*

# Message helper
msg = @echo ">>> $(1) <<<"

# Phony targets
.PHONY: all shell clean-shell brew clean-brew alacritty clean-alacritty emacs clean-emacs firefox firefox-config clean-firefox nvim clean-nvim tmux clean-tmux clean help

# Default target
all: shell brew alacritty emacs firefox nvim tmux
	$(call msg,"Setup complete! Run 'make firefox-config' after Firefox initializes.")

# Help target
help:
	@echo "Available targets:"
	@echo "  all                : Install all components (shell, brew, alacritty, emacs, firefox, nvim, tmux)"
	@echo "  shell              : Install and configure shell with Prezto"
	@echo "  clean-shell        : Remove shell configuration and Prezto directory"
	@echo "  brew               : Install or update Homebrew and bundle dependencies"
	@echo "  clean-brew         : Remove brew configuration (Homebrew uninstallation is optional)"
	@echo "  alacritty          : Install and configure Alacritty"
	@echo "  clean-alacritty    : Uninstall Alacritty and remove configuration"
	@echo "  emacs              : Install and configure Emacs"
	@echo "  clean-emacs        : Uninstall Emacs and remove configuration"
	@echo "  firefox            : Install Firefox, initialize profile, and stow application settings"
	@echo "  firefox-config     : Configure Firefox profile with custom CSS"
	@echo "  clean-firefox      : Uninstall Firefox and remove stowed settings"
	@echo "  nvim               : Install and configure Neovim"
	@echo "  clean-nvim         : Uninstall Neovim and remove configuration"
	@echo "  tmux               : Configure Tmux"
	@echo "  clean-tmux         : Remove Tmux configuration"
	@echo "  clean              : Remove all installed configurations (use with caution)"
	@echo "  help               : Show this help message"

# Directory creation
$(CONFIG_DIR):
	@mkdir -p $@

# Homebrew management
brew:
	$(call msg,"Managing Homebrew")
	@if command -v brew >/dev/null 2>&1; then \
		brew update && brew upgrade && brew upgrade --cask; \
	else \
		/bin/bash -c "$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" || { echo "Homebrew install failed"; exit 1; }; \
	fi
	@brew bundle --file=$(BREWFILE) || { echo "Brew bundle failed"; exit 1; }
	@stow --dotfiles $(STOW_OPTS) --target=$(HOME) brew

clean-brew:
	$(call msg,"Cleaning Brew configuration")
	@stow -D --dotfiles $(STOW_OPTS) --target=$(HOME) brew
	@echo "Note: To fully uninstall Homebrew and its dependencies, run '/bin/bash -c \"$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh)\"'"

# Shell setup (Prezto)
shell: $(CONFIG_DIR) brew
	$(call msg,"Setting up shell with Prezto")
	@if [ ! -d $(PREZTO_DIR) ]; then \
		git clone --depth 1 --recursive https://github.com/sorin-ionescu/prezto.git $(PREZTO_DIR) || exit 1; \
	else \
		cd $(PREZTO_DIR) && git pull && git submodule sync --recursive && git submodule update --init --recursive; \
	fi
	@stow --dotfiles $(STOW_OPTS) --target=$(HOME) dotfiles

clean-shell:
	$(call msg,"Cleaning shell configuration")
	@stow -D --dotfiles $(STOW_OPTS) --target=$(HOME) dotfiles
	@if [ -d $(PREZTO_DIR) ]; then \
		rm -rf $(PREZTO_DIR); \
		echo "Prezto directory removed"; \
	fi

# Alacritty
alacritty: shell tmux
	$(call msg,"Installing Alacritty")
	@brew install --cask alacritty
	@if [ "$$(uname)" = "Darwin" ]; then \
		xattr -r -d com.apple.quarantine /Applications/Alacritty.app; \
	fi
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) alacritty

clean-alacritty:
	$(call msg,"Cleaning Alacritty")
	@brew uninstall --cask --zap alacritty 2>/dev/null || true
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) alacritty

# Emacs
emacs: $(EMACS_CONFIG_DIR) shell
	$(call msg,"Installing Emacs")
	@brew install --cask emacs
	@brew install gnupg
	@stow --no-folding --dotfiles $(STOW_OPTS) --target=$(HOME) emacs

clean-emacs:
	$(call msg,"Cleaning Emacs")
	@brew uninstall --cask --zap emacs 2>/dev/null || true
	@brew uninstall gnupg 2>/dev/null || true
	@stow -D --no-folding --dotfiles $(STOW_OPTS) --target=$(HOME) emacs
	@rm -rf $(EMACS_CONFIG_DIR) 2>/dev/null || true

$(EMACS_CONFIG_DIR):
	@mkdir -p $@

# Firefox Installation and Configuration
firefox: shell
	$(call msg,"Installing Firefox and initializing profile")
	@brew install --cask firefox
	@if [ "$$(uname)" = "Darwin" ]; then \
		xattr -r -d com.apple.quarantine /Applications/Firefox.app; \
	fi
	@stow $(STOW_OPTS) -d firefox --target=/Applications/Firefox.app/Contents/Resources/ settings
# Launch Firefox briefly to create a default profile
	@/Applications/Firefox.app/Contents/MacOS/firefox --headless & \
	FIREFOX_PID=$$!; \
	sleep 5; \
	kill $$FIREFOX_PID 2>/dev/null || true
	@echo "Firefox installed and profile initialized. Run 'make firefox-config' to apply customizations."

firefox-config:
	$(call msg,"Configuring Firefox")
	@FIREFOX_PROFILE=$$(grep -A1 '^\[Install' "${HOME}/Library/Application Support/Firefox/profiles.ini" | grep 'Default=' | cut -d'/' -f2); \
	if [ -z "$$FIREFOX_PROFILE" ]; then \
		echo "No default profile found. Please ensure Firefox has initialized a profile."; \
		exit 1; \
	fi; \
	FIREFOX_PROFILE_PATH="${HOME}/Library/Application Support/Firefox/Profiles/$$FIREFOX_PROFILE"; \
	mkdir -p "$$FIREFOX_PROFILE_PATH/chrome"; \
	cp -R ./firefox/chrome/* "$$FIREFOX_PROFILE_PATH/chrome/"; \
	echo "Firefox configuration applied to $$FIREFOX_PROFILE_PATH/chrome"

clean-firefox:
	$(call msg,"Cleaning Firefox")
	@stow -D $(STOW_OPTS) -d firefox --target=/Applications/Firefox.app/Contents/Resources/ settings || true
	@brew uninstall --cask --zap firefox 2>/dev/null || true

# Neovim
nvim: shell
	$(call msg,"Installing Neovim")
	@brew install neovim
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) nvim

clean-nvim:
	$(call msg,"Cleaning Neovim")
	@brew uninstall neovim 2>/dev/null || true
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) nvim
	@rm -rf $(HOME)/.local/share/nvim $(HOME)/.local/state/nvim 2>/dev/null || true

# Tmux
tmux: shell
	$(call msg,"Configuring Tmux")
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) tmux

clean-tmux:
	$(call msg,"Cleaning Tmux")
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) tmux

# Full cleanup
# WARNING: This will remove all installed configurations and may delete user data.
clean: clean-alacritty clean-emacs clean-firefox clean-nvim clean-tmux clean-shell clean-brew
	$(call msg,"Full cleanup complete")
