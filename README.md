<p align="center">Slava's bootstrap repo</p>

<p align="center">
  <img src="https://img.shields.io/badge/platform-macos-lightgrey.svg"/>
</p>

---

My personal macOS dev environment. Clone it, run `make`, and a fresh machine is ready to go.

It uses [GNU Stow](https://www.gnu.org/software/stow/) to symlink configs into the right places and a Makefile to wire everything together.

## What's inside

- **Shell** — Zsh with a modular config (no framework), [Starship](https://starship.rs) prompt, fzf, bat, eza, direnv
- **Terminal** — [Ghostty](https://ghostty.org), iTerm2 as fallback, Tmux for multiplexing
- **Editors** — Neovim (Lua config, lazy.nvim, LSP for Go, Python, Rust, TypeScript, F#), Emacs, Vim
- **Packages** — Homebrew + Brewfile (CLI tools, casks, App Store apps via mas)
- **Browsers** — Firefox with custom CSS, Brave
- **Theme** — One Dark Pro Max, consistently applied across terminal, prompt, editor, and tmux

## Usage

```sh
# Full setup (shell + brew + ghostty + tmux)
make

# Individual components
make nvim
make emacs
make firefox

# Tear down a component
make clean-firefox
```

Each target handles installing dependencies, symlinking configs, and any tool-specific setup (e.g. Firefox profile initialization).

<p align="center">Copyright &copy; 2019-present Slava Buzin</p>

<p align="center">
  <a href="https://github.com/vbuzin/dotfiles/blob/master/LICENSE.md">
    <img src="https://img.shields.io/badge/License-MIT-lightgrey.svg"/>
  </a>
</p>
