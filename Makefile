PATH    := $(PATH):/opt/homebrew/bin
SHELL   := env PATH=$(PATH) /bin/bash
CFG_DIR := $(abspath $(HOME)/.config)
$(shell mkdir -p $(CFG_DIR))

message  = " >>> ==================== "$(1)" ==================== <<< "

# all
# ==============================================================================
all: shell brew
.PHONY: alacritty _alacritty emacs _emacs firefox firefox-cfg _firefox nvim _nvim tmux _tmux
# shell
# ==============================================================================
PREZTO_DIR := $(abspath $(HOME)/.zprezto)

shell: brew
	@echo $(call message,"Installing prezto and submodules")
	@if [ ! -d $(PREZTO_DIR) ]; then \
		@git clone --depth 1 --recursive https://github.com/sorin-ionescu/prezto.git $(PREZTO_DIR); \
	fi
	@cd $(PREZTO_DIR) && git pull && \
	git submodule sync --recursive && \
	git submodule update --init --recursive

	@stow --dotfiles --ignore=.DS_Store --override=.* --target=${HOME} dotfiles

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
	@brew bundle --file Brewfile
	@stow --dotfiles --ignore=.DS_Store --override=.* --target=${HOME} brew

# alacritty
# ==============================================================================
alacritty: shell tmux
	@echo $(call message,"Installing and configuring Alacritty")
	@brew install alacritty --cask
	@ALACRITTY_APP=/Applications/Alacritty.app && \
			xattr -r -d com.apple.quarantine $$ALACRITTY_APP
	@stow --ignore=.DS_Store --override=.* --target=${HOME}/.config alacritty

_alacritty:
	@echo $(call message,"Uninstalling alacritty")
	@-brew uninstall --cask --zap alacritty
	@brew autoremove
	@stow -D --ignore=.DS_Store --target=${HOME}/.config alacritty

# emacs
# ==============================================================================
emacs: shell
	@echo $(call message,"Installing and configuring Emacs")
	@brew install --cask emacs
	@brew install gnupg
	@stow --dotfiles --target=${HOME} emacs

_emacs:
	@echo $(call message,"Installing and configuring Emacs")
	@brew uninstall --zap emacs
	@brew uninstall --zap gnupg
	@brew autoremove
	@stow -D --dotfiles --target=${HOME} emacs

# firefox
# ==============================================================================
firefox: shell
	@echo $(call message,"Installing and configuring Firefox")
	@brew install --cask firefox
	@FIREFOX_APP=/Applications/Firefox.app && \
		xattr -r -d com.apple.quarantine $$FIREFOX_APP && \
		stow --ignore=.DS_Store --override=.* -d firefox --target=$$FIREFOX_APP/Contents/Resources/ settings

# linking doesn't work for some reason
firefox-cfg:
	@echo $(call message,"Provisioning userChrome and userContent")
	@FIREFOX_PROFILE=$$(grep -A1 '\[Install' "${HOME}/Library/Application Support/Firefox/profiles.ini" | grep 'Default=' | cut -d'/' -f2) && \
		FIREFOX_PROFILE_PATH="${HOME}/Library/Application Support/Firefox/Profiles/$$FIREFOX_PROFILE" && \
		cp -R ./firefox/chrome "$$FIREFOX_PROFILE_PATH"/

_firefox:
	@echo $(call message,"Uninstalling Firefox")
	@brew uninstall --zap --cask firefox
	@brew autoremove

# nvim
# ==============================================================================
nvim: shell
	@echo $(call message,"Installing and configuring Nvim")
	@brew install neovim
	@stow --ignore=.DS_Store --override=.* --target=${HOME}/.config nvim

_nvim:
	@echo $(call message,"Unistalling Nvim and dependencies")
	@brew uninstall neovim
	@brew autoremove
	@stow -D --ignore=.DS_Store --target=${HOME}/.config nvim
	@rm -rf ${HOME}/.local

# tmux
# ==============================================================================
tmux: shell
	@echo $(call message,"Configuring tmux")
	@stow --ignore=.DS_Store --override=.* --target=${HOME}/.config tmux

_tmux:
	@echo $(call message,"Uninstalling tmux")
	@stow -D --ignore=.DS_Store --target=${HOME}/.config tmux
