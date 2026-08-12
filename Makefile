# Variables
PATH             := $(PATH):/opt/homebrew/bin
SHELL            := env PATH=$(PATH) /bin/bash
CONFIG_DIR       := $(HOME)/.config
EMACS_CONFIG_DIR := $(HOME)/.emacs.d
BREWFILE         := $(CURDIR)/Brewfile
DEVTOOLS_BREWFILE := $(CURDIR)/devtools/Brewfile
# Shared language toolchain (editor-agnostic). Keep in sync with devtools/Brewfile.
DEVTOOLS_FORMULAE := rustup python deno lua-language-server stylua basedpyright ruff taplo
# Stamp so `make update` can refresh toolchains only when installed.
DEVTOOLS_STAMP   := $(HOME)/.local/state/bootstrap/dev-tools
STOW_OPTS        := --ignore=.DS_Store --override=.*

# Message helper
msg = @echo ">>> $(1) <<<"

# Phony targets
.PHONY: all update shell clean-shell brew clean-brew ghostty clean-ghostty opencode clean-opencode emacs clean-emacs firefox firefox-config clean-firefox dev-tools update-dev-tools clean-dev-tools clean-dev-tools-hard verify-dev-tools nvim clean-nvim tmux clean-tmux zed clean-zed helix clean-helix lazygit clean-lazygit leaf clean-leaf macos clean help nvim-cheatsheet nvim-cheatsheet-screen nvim-cheatsheet-print

# Default target
all: shell brew ghostty tmux
	$(call msg,"Setup complete! Run 'make firefox-config' after Firefox initializes.")

# Help target
help:
	@echo "Available targets:"
	@echo "  all                : Install all components (shell, brew, ghostty, tmux)"
	@echo "  update             : Upgrade installed Homebrew packages + dev-tools if present"
	@echo "  shell              : Install and configure shell (starship + plugins)"
	@echo "  clean-shell        : Remove shell configuration"
	@echo "  brew               : Install or update Homebrew and bundle dependencies"
	@echo "  clean-brew         : Remove brew configuration"
	@echo "  ghostty            : Install and configure Ghostty"
	@echo "  clean-ghostty      : Uninstall Ghostty and remove configuration"
	@echo "  opencode           : Install and configure Opencode"
	@echo "  clean-opencode     : Uninstall Opencode and remove configuration"
	@echo "  emacs              : Install and configure Emacs"
	@echo "  clean-emacs        : Uninstall Emacs and remove configuration"
	@echo "  firefox            : Install Firefox, initialize profile, and stow application settings"
	@echo "  firefox-config     : Configure Firefox profile with custom CSS"
	@echo "  clean-firefox      : Uninstall Firefox and remove stowed settings"
	@echo "  dev-tools          : Install shared language toolchain (devtools/Brewfile + rustup)"
	@echo "  update-dev-tools   : Upgrade shared language toolchain"
	@echo "  clean-dev-tools    : Uninstall toolchain formulae (keeps ~/.rustup ~/.cargo)"
	@echo "  clean-dev-tools-hard : clean-dev-tools + wipe ~/.rustup ~/.cargo"
	@echo "  nvim               : Install Neovim + config (soft: run make dev-tools for LSPs)"
	@echo "  clean-nvim         : Uninstall Neovim config only (does not remove dev-tools)"
	@echo "  tmux               : Configure Tmux"
	@echo "  clean-tmux         : Remove Tmux configuration"
	@echo "  zed                : Install and configure Zed (soft: run make dev-tools for PATH LSPs)"
	@echo "  clean-zed          : Uninstall Zed and remove configuration"
	@echo "  helix              : Install and configure Helix"
	@echo "  clean-helix        : Uninstall Helix and remove configuration"
	@echo "  lazygit            : Install and configure Lazygit"
	@echo "  clean-lazygit      : Uninstall Lazygit and remove configuration"
	@echo "  leaf               : Install and configure Leaf (markdown viewer)"
	@echo "  clean-leaf         : Uninstall Leaf and remove configuration"
	@echo "  macos              : Apply macOS system preferences (requires sudo)"
	@echo "  clean              : Remove all installed configurations (use with caution)"
	@echo "  help               : Show this help message"
	@echo "  nvim-cheatsheet        : Build both cheatsheet PDFs (screen + print) via Docker"
	@echo "  nvim-cheatsheet-screen : Build dark-background PDF for screen viewing"
	@echo "  nvim-cheatsheet-print  : Build white-background PDF for printing"

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

# Upgrade whatever is already installed. Dev toolchain extras only if stamp/formula present.
update:
	$(call msg,"Updating installed packages")
	@if ! command -v brew >/dev/null 2>&1; then \
		echo "Homebrew not installed; nothing to update."; \
		exit 0; \
	fi
	@brew update
	@brew upgrade
	@brew upgrade --cask
	@brew cleanup --prune=all
	@if [ -f "$(DEVTOOLS_STAMP)" ] || brew list --formula deno >/dev/null 2>&1; then \
		$(MAKE) update-dev-tools; \
	else \
		echo ">>> dev-tools not installed; skipping toolchain extras <<<"; \
	fi

clean-brew:
	$(call msg,"Cleaning Brew configuration")
	@/bin/bash -c \"$$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh)\"

# Shell setup (starship + zsh-autosuggestions + zsh-syntax-highlighting)
shell: $(CONFIG_DIR) brew
	$(call msg,"Setting up shell with Starship")
	@stow --dotfiles $(STOW_OPTS) --target=$(HOME) dotfiles
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) zsh
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) starship
	@$(MAKE) ghostty
	@$(MAKE) tmux

clean-shell:
	$(call msg,"Cleaning shell configuration")
	@stow -D --dotfiles $(STOW_OPTS) --target=$(HOME) dotfiles
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) zsh
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) starship

# Ghostty
ghostty:
	$(call msg,"Installing Ghostty")
	@brew install --cask ghostty
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) ghostty

clean-ghostty:
	$(call msg,"Cleaning Ghostty")
	@brew uninstall --cask --zap ghostty 2>/dev/null || true
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) ghostty

# Opencode
opencode:
	$(call msg,"Installing Opencode")
	@brew install oven-sh/bun/bun
	@bun add -g opencode-ai
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) opencode

clean-opencode:
	$(call msg,"Cleaning Opencode")
	@rm -rf $(CONFIG_DIR)/opencode/.gitignore
	@rm -rf $(CONFIG_DIR)/opencode/bun.lock
	@rm -rf $(CONFIG_DIR)/opencode/package.json
	@rm -rf $(CONFIG_DIR)/opencode/package-lock.json
	@rm -rf $(CONFIG_DIR)/opencode/node_modules 2>/dev/null || true
	@bun rm -g opencode-ai
	@brew rm oven-sh/bun/bun
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) opencode

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

# Shared language toolchain (editor-agnostic: nvim, Zed, shell)
dev-tools: brew
	$(call msg,"Installing dev tools")
	@brew bundle --file=$(DEVTOOLS_BREWFILE) || { echo "devtools Brewfile bundle failed"; exit 1; }
	@export PATH="$$(brew --prefix rustup)/bin:$$PATH"; \
		rustup default stable 2>/dev/null || rustup toolchain install stable; \
		rustup component add rust-analyzer rustfmt clippy
	@mkdir -p "$$(dirname $(DEVTOOLS_STAMP))"
	@date -u +%Y-%m-%dT%H:%M:%SZ > "$(DEVTOOLS_STAMP)"
	@$(MAKE) verify-dev-tools

update-dev-tools:
	$(call msg,"Updating dev tools")
	@if ! command -v brew >/dev/null 2>&1; then \
		echo "Homebrew not installed"; exit 1; \
	fi
	@brew bundle --file=$(DEVTOOLS_BREWFILE) || { echo "devtools Brewfile bundle failed"; exit 1; }
	@for f in $(DEVTOOLS_FORMULAE); do \
		if brew list --formula "$$f" >/dev/null 2>&1; then \
			brew upgrade "$$f" 2>/dev/null || true; \
		fi; \
	done
	@if brew list --formula rustup >/dev/null 2>&1; then \
		export PATH="$$(brew --prefix rustup)/bin:$$PATH"; \
		rustup self update 2>/dev/null || true; \
		rustup update stable; \
		rustup component add rust-analyzer rustfmt clippy; \
	fi
	@mkdir -p "$$(dirname $(DEVTOOLS_STAMP))"
	@date -u +%Y-%m-%dT%H:%M:%SZ > "$(DEVTOOLS_STAMP)"
	@$(MAKE) verify-dev-tools

clean-dev-tools:
	$(call msg,"Cleaning dev tools")
	@for f in $(DEVTOOLS_FORMULAE); do \
		if brew list --formula "$$f" >/dev/null 2>&1; then \
			echo "uninstalling $$f"; \
			brew uninstall "$$f" || exit 1; \
		fi; \
	done
	@brew autoremove -v 2>/dev/null || true
	@rm -f "$(DEVTOOLS_STAMP)"
	@# Leftover nvim-owned DAP installs from the previous layout
	@rm -rf $(HOME)/.local/share/nvim/tools

clean-dev-tools-hard: clean-dev-tools
	$(call msg,"Wiping Rust toolchain homes")
	@rm -rf $(HOME)/.rustup $(HOME)/.cargo

verify-dev-tools:
	$(call msg,"Verifying dev tools")
	@export PATH="$$(brew --prefix rustup 2>/dev/null)/bin:$$HOME/.cargo/bin:$$PATH"; \
		command -v deno >/dev/null || { echo "missing deno"; exit 1; }; \
		command -v stylua >/dev/null || { echo "missing stylua"; exit 1; }; \
		command -v lua-language-server >/dev/null || { echo "missing lua-language-server"; exit 1; }; \
		command -v basedpyright >/dev/null || { echo "missing basedpyright"; exit 1; }; \
		command -v ruff >/dev/null || { echo "missing ruff"; exit 1; }; \
		command -v taplo >/dev/null || { echo "missing taplo"; exit 1; }; \
		command -v rust-analyzer >/dev/null || { echo "missing rust-analyzer"; exit 1; }; \
		command -v rustfmt >/dev/null || { echo "missing rustfmt"; exit 1; }; \
		echo ">>> all dev tools OK <<<"

# Neovim — editor only (language tools: make dev-tools)
nvim: shell
	$(call msg,"Installing Neovim")
	@brew install neovim tree-sitter-cli
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) nvim

clean-nvim:
	$(call msg,"Cleaning Neovim")
	@brew uninstall neovim tree-sitter-cli 2>/dev/null || true
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) nvim 2>/dev/null || true
	@rm -rf $(HOME)/.local/share/nvim $(HOME)/.local/state/nvim 2>/dev/null || true

# Zed — editor only (language tools on PATH from make dev-tools)
zed: brew
	$(call msg,"Installing Zed")
	@brew install --cask zed
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) zed

clean-zed:
	$(call msg,"Cleaning Zed")
	@brew uninstall --cask --zap zed 2>/dev/null || true
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) zed

# Helix
helix: brew
	$(call msg,"Installing Helix")
	@brew install helix
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) helix

clean-helix:
	$(call msg,"Cleaning Helix")
	@brew uninstall helix 2>/dev/null || true
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) helix

# Lazygit
lazygit: brew
	$(call msg,"Installing Lazygit")
	@brew install lazygit
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) lazygit

clean-lazygit:
	$(call msg,"Cleaning Lazygit")
	@brew uninstall lazygit 2>/dev/null || true
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) lazygit

# Leaf (markdown viewer)
leaf: brew
	$(call msg,"Installing Leaf")
	@brew install leaf-markdown-viewer
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) leaf

clean-leaf:
	$(call msg,"Cleaning Leaf")
	@brew uninstall leaf-markdown-viewer 2>/dev/null || true
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) leaf

# Tmux
tmux:
	$(call msg,"Installing and configuring Tmux")
	@brew install tmux
	@stow $(STOW_OPTS) --target=$(CONFIG_DIR) tmux

clean-tmux:
	$(call msg,"Cleaning Tmux")
	@brew uninstall tmux
	@stow -D $(STOW_OPTS) --target=$(CONFIG_DIR) tmux

# Neovim cheatsheet (requires Docker — first run pulls texlive/texlive:latest ~5 GB)
nvim-cheatsheet: nvim-cheatsheet-screen nvim-cheatsheet-print

nvim-cheatsheet-screen:
	$(call msg,"Compiling nvim cheatsheet -- screen PDF")
	@bash docs/compile.sh screen

nvim-cheatsheet-print:
	$(call msg,"Compiling nvim cheatsheet -- print PDF")
	@bash docs/compile.sh print

# macOS system configuration
macos:
	$(call msg,"Configuring macOS")
	@defaults write com.apple.dock tilesize -int 45
	@defaults write com.apple.dock autohide -bool true
	@defaults write com.apple.dock show-recents -bool false
	@defaults write com.apple.dock mineffect -string scale
	@defaults write com.apple.dock minimize-to-application -bool true
	@defaults write com.apple.dock expose-group-apps -bool true
	@defaults write com.apple.dock wvous-bl-corner -int 14
	@defaults write com.apple.dock wvous-bl-modifier -int 1048576
	@defaults write com.apple.dock wvous-br-corner -int 1
	@defaults write com.apple.dock wvous-br-modifier -int 0
	@defaults write NSGlobalDomain AppleInterfaceStyle Dark
	@defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
	@defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false
	@defaults write NSGlobalDomain NSAutomaticSpellingCorrectionEnabled -bool false
	@defaults write com.apple.finder FXPreferredViewStyle -string Nlsv
	@defaults write com.apple.finder NewWindowTarget -string PfAF
	@defaults write com.apple.menuextra.clock ShowAMPM -bool true
	@defaults write com.apple.menuextra.clock ShowDayOfWeek -bool true
	@defaults write com.apple.menuextra.clock ShowDate -bool false
	@defaults write com.apple.TextEdit RichText -bool false
	@killall Dock
	@killall Finder
	@killall SystemUIServer
	@sudo scutil --set LocalHostName vbmacp
	@sudo nvram StartupMute=%01
	@sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on
	@sudo cp /etc/pam.d/sudo_local.template /etc/pam.d/sudo_local
	@sudo sed -i '' '/pam_tid.so/s/^#[[:space:]]*//' /etc/pam.d/sudo_local

# Full cleanup
# WARNING: This will remove all installed configurations and may delete user data.
clean: clean-ghostty clean-opencode clean-emacs clean-firefox clean-nvim clean-zed clean-tmux clean-helix clean-lazygit clean-leaf clean-dev-tools clean-shell clean-brew
	$(call msg,"Full cleanup complete")
