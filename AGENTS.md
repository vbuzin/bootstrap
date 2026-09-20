# Bootstrap

Personal macOS environment. Packages live at the repo root and are installed with GNU Stow plus the Makefile.

## Layout

- Most packages are `foo/foo/` stowed into `~/.config/foo` (`stow --target=~/.config foo`).
- `dotfiles/` stows into `$HOME` with `--dotfiles`.
- `emacs/` stows into `$HOME` with `--dotfiles --no-folding`.
- `grok/grok/` stows into `~/.grok` with `--no-folding`. Grok's home is `~/.grok`, not XDG. Never set `GROK_HOME` to a symlink or to `~/.config/grok` (sandbox refuses a symlinked Grok home).

## Coupling

- Base `Brewfile` is lifestyle only (shell, casks, fonts, stow). Optional stacks (`make grok`, `make nvim`, `make emacs`, …) install their own formulae in the target, not here.
- Language tools are `make dev-tools` / `devtools/Brewfile`.
- Reject mise/asdf. Prefer Plan Mode for non-trivial multi-concern changes.

## Grok

- User config, global `AGENTS.md`, skills, and hooks live in `grok/grok/` and are available in every project after `make grok`. `config.toml` is copied (Grok rewrites it and would break a symlink); the rest are stowed.
- Bundled CLI skills stay with the CLI; do not copy `~/.grok/bundled/`.
- Desktop banners are `hooks/notify.json` (relative path to `bin/notify-macos.py`). Do not use `[ui.notifications.hooks]`. Keep `[ui.notifications] method = "none"`.
- Theme is `terminal` (needs `[features] terminal_theme = true`) so chrome uses Ghostty's One Dark Pro Max palette. Grok has no user theme files; code-block syntax still uses a bundled `.tmTheme`.
- `make clean-grok` unstows and zaps the cask. It does not delete `~/.grok` (sessions, auth, memory).
- First launch on a new machine still needs `grok login`.

## Editors

- Maximise reuse of PATH-provided LSPs.
- Minimal configs: only essential, non-duplicative settings.
