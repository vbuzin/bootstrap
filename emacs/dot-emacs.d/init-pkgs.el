;;; init-pkgs.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Package System Configuration
;; =============================================================================
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(require 'package)

(setq use-package-enable-imenu-support t)
(require 'use-package-ensure)

(setq package-archives
      '(("gnu"     . "https://elpa.gnu.org/packages/")
        ("nongnu"  . "https://elpa.nongnu.org/nongnu/")
        ("melpa"   . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu"     . 10)
        ("melpa"   .  7)
        ("nongnu"  .  5)))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-defer t
      use-package-always-ensure t)


;;; Core Packages
;; =============================================================================

;; Minibuffer Completion (Vertico + Consult + Marginalia + Orderless)
;; -----------------------------------------------------------------------------
(use-package vertico
  :demand t
  :config (vertico-mode)
  :custom
  (vertico-count 12)
  (vertico-cycle t))

(use-package consult
  :bind
  (([remap switch-to-buffer]   . consult-buffer)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap yank-pop]           . consult-yank-pop)
   ([remap apropos-command]    . consult-apropos)
   ("s-b" . consult-buffer)
   ("s-r" . consult-recent-file)
   ("s-l" . consult-line)
   ("s-L" . consult-line-multi)
   ("s-g" . consult-ripgrep)
   ;; Search/navigate prefix (C-c s)
   ("C-c s i" . consult-imenu)
   ("C-c s I" . consult-imenu-multi)
   ("C-c s o" . consult-outline)
   ("C-c s m" . consult-mark)
   ("C-c s M" . consult-global-mark)
   ("C-c s e" . consult-flymake)
   ("C-c s y" . consult-yank-pop))
  :config
  (setq consult-narrow-key "<"))

(use-package marginalia
  :demand t
  :config (marginalia-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot (styles orderless basic)))))

;; In-buffer Completion (Corfu + Cape) — replaces company-mode
;; -----------------------------------------------------------------------------
(use-package corfu
  :demand t
  :config
  (global-corfu-mode)
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if completion is expected."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
         ("s-/" . corfu-complete)
         ("RET" . nil)))

(use-package completion-preview
  :ensure nil
  :hook ((prog-mode text-mode) . completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
              ("M-<tab>" . completion-preview-insert)
              ("M-n"     . completion-preview-next-candidate)
              ("M-p"     . completion-preview-prev-candidate)))

(use-package cape
  :demand t                          ; ← needs to register capfs early
  :config
  ;; Add Cape backends buffer-locally so they don't pollute every buffer type.
  (defun my/cape-prog-setup ()
    (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
    (add-hook 'completion-at-point-functions #'cape-file    nil t)
    (add-hook 'completion-at-point-functions #'cape-keyword nil t))
  (defun my/cape-text-setup ()
    (add-hook 'completion-at-point-functions #'cape-dabbrev nil t)
    (add-hook 'completion-at-point-functions #'cape-dict    nil t))
  (add-hook 'prog-mode-hook #'my/cape-prog-setup)
  (add-hook 'text-mode-hook #'my/cape-text-setup))

;; Dired
;; -----------------------------------------------------------------------------
(use-package dired
  :ensure nil
  :hook (dired-mode . (lambda () (require 'dired-x)))
  :config
  (setq dired-dwim-target t
        dired-reuse-buffer t
        dired-recursive-copies 'top
        dired-recursive-deletes 'top
        ls-lisp-dirs-first t
        ls-lisp-use-insert-directory-program nil
        wdired-allow-to-change-permissions t))

;; ibuffer — grouped buffer list
;; -----------------------------------------------------------------------------
(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-switch-to-saved-filter-groups "default")))
  :config
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Org"   (mode . org-mode))
           ("Rust"  (mode . rust-ts-mode))
           ("Dired" (mode . dired-mode))
           ("Magit" (derived-mode . magit-mode))
           ("Help"  (or (mode . help-mode) (mode . Info-mode)))
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*"))))))
  (setq ibuffer-show-empty-filter-groups nil))

;; Modeline
;; -----------------------------------------------------------------------------
(use-package doom-modeline
  :demand t
  :config (doom-modeline-mode t)
  (setq doom-modeline-height 24
        doom-modeline-hud t
        doom-modeline-icon nil
        doom-modeline-support-imenu t))

;; Themes
;; -----------------------------------------------------------------------------
;; doom-one + One Dark Pro Max tweaks (same overrides as nvim onedarkpro):
;;   bg #080909, comment #7F838C, red→#E15687 (vars/diagnostics)
;; https://github.com/bukitoka/one-dark-pro-max
(use-package doom-themes
  :demand t                          ; ← override global defer
  :config                            ; ← moved (load-theme) from :init to :config
  (load-theme 'doom-one t)
  (custom-set-faces
   ;; One Dark Pro Max base tweaks (match nvim onedarkpro overrides)
   `(default ((t :background "#080909" :foreground "#abb2bf")))
   `(font-lock-comment-face ((t :foreground "#7F838C")))
   `(font-lock-variable-name-face ((t :foreground "#E15687")))
   `(error ((t :foreground "#E15687")))
   ;; Un-mute doom-one blends so treesit level-4 faces actually read
   ;; (function-call / property / variable-use are washed out by default).
   `(font-lock-function-call-face ((t :foreground "#61afef" :slant normal)))
   `(font-lock-property-name-face ((t :foreground "#E15687" :weight normal)))
   `(font-lock-property-use-face ((t :foreground "#E15687")))
   `(font-lock-variable-use-face ((t :foreground "#E15687")))
   `(help-key-binding ((t :box nil)))
   `(fringe ((t (:inherit default :foreground ,(face-attribute 'default :foreground)))))
   `(line-number-current-line ((t (:inherit default :foreground ,(face-attribute 'default :foreground) :weight normal))))
   `(Info-quoted ((t (:inherit font-lock-comment-face :slant italic)))))
  (doom-themes-org-config))

;; Utility Packages
;; -----------------------------------------------------------------------------
(use-package avy
  :bind (("s-\\" . avy-goto-char-2)
         ("s-j" . avy-goto-line)))

(use-package expand-region
  :bind (("s-=" . er/expand-region)
         ("s-M-=" . er/contract-region)))

(use-package fringe-current-line
  :demand t
  :config (global-fringe-current-line-mode t))

(use-package move-text
  :bind (("M-s-<up>"   . move-text-up)
         ("M-s-<down>" . move-text-down))
  :config
  (dolist (command '(move-text-up move-text-down))
    (advice-add command :after
                (lambda (&rest _)
                  (let ((deactivate deactivate-mark))
                    (if (region-active-p)
                        (indent-region (region-beginning) (region-end))
                      (indent-region (line-beginning-position) (line-end-position)))
                    (setq deactivate-mark deactivate))))))

(use-package shrink-whitespace
  :bind ("C-c ." . shrink-whitespace))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 80))

(use-package vlf
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))

;;; Development
;; =============================================================================
;; -----------------------------------------------------------------------------
(setq treesit-font-lock-level 4)

;; Eldoc (built-in) — richer ambient docs from eglot hover/signature
;; -----------------------------------------------------------------------------
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.2)
  ;; Allow multi-line hover/signature in the echo area (default truncates hard).
  (eldoc-echo-area-use-multiline-p 5)
  (eldoc-echo-area-prefer-doc-buffer 'maybe)
  (eldoc-echo-area-display-truncation-message nil)
  :config
  ;; Room for a few lines of hover without dominating the frame.
  (setq max-mini-window-height 0.25))

;; Eglot (built-in LSP client) — replaces lsp-mode
;; -----------------------------------------------------------------------------
(use-package eglot
  :ensure nil
  :hook ((rust-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format-buffer)
         ;; Full hover docs in a buffer (ambient eldoc still uses the echo area).
         ("C-c l d" . eldoc-print-current-symbol-info)
         ("C-c l i" . eglot-find-implementation)
         ("C-c l t" . eglot-find-typeDefinition)
         ("C-c l h" . eglot-inlay-hints-mode))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  :config
  ;; Performance: don't log jsonrpc events
  (fset #'jsonrpc--log-event #'ignore)

  ;; Format via eglot (LSP) on save for any eglot-managed buffer (works for Rust, etc.)
  (defun my/eglot-format-on-save ()
    "Format buffer via eglot before saving, if eglot is active."
    (when (bound-and-true-p eglot--managed-mode)
      (eglot-format-buffer)))
  (add-hook 'before-save-hook #'my/eglot-format-on-save)

  (add-to-list 'eglot-server-programs
    '((rust-ts-mode rust-mode rustic-mode) .
      ("rust-analyzer"
       :initializationOptions
       (:cargo (:allFeatures t)
        :check (:command "clippy"
                :features "all"
                :extraArgs ["--no-deps"])
        :procMacro (:enable t)
        :inlayHints
        (:closureReturnTypeHints (:enable t)
         :lifetimeElisionHints (:enable "skip_trivial"
                                 :useParameterNames :json-false)
         :parameterHints (:enable t)
         :typeHints (:enable t))))))

  ;; Also send as workspace config (live updates without full restart).
  (setq-default eglot-workspace-configuration
                '(:rust-analyzer
                  (:cargo (:allFeatures t)
                   :check (:command "clippy"
                           :features "all"
                           :extraArgs ["--no-deps"])
                   :inlayHints
                   (:closureReturnTypeHints (:enable t)
                    :lifetimeElisionHints (:enable "skip_trivial"
                                            :useParameterNames :json-false)))))

  ;; Inlay hints (Emacs 29+ built-in eglot support)
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)

  ;; Prefer tree-sitter over RA semantic tokens for Rust (avoids flicker; matches
  ;; the rationale in the user's nvim rustaceanvim config).
  (defun my/eglot-prefer-treesitter-for-rust ()
    (when (derived-mode-p 'rust-ts-mode 'rust-mode 'rustic-mode)
      (setq-local eglot-ignored-server-capabilities
                  (cons :semanticTokensProvider
                        (or eglot-ignored-server-capabilities '())))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-prefer-treesitter-for-rust)

  ;; Subword navigation (helpful for snake_case and mixed-case identifiers)
  (add-hook 'rust-ts-mode-hook #'subword-mode))

;; Flymake (built-in) — eglot feeds LSP diagnostics into it when managing a buffer
;; -----------------------------------------------------------------------------
(use-package flymake
  :ensure nil
  :init
  (defun my/eglot-will-manage-p ()
    "Non-nil if `eglot-ensure' is on this major mode's hook."
    (let ((hook (intern (format "%s-hook" major-mode))))
      (and (boundp hook)
           (seq-some (lambda (fn)
                       (eq (if (consp fn) (car fn) fn) #'eglot-ensure))
                     (symbol-value hook)))))

  (defun my/maybe-enable-flymake ()
    "Enable Flymake in programming buffers where Eglot won't own it.

Eglot turns Flymake on itself and replaces `flymake-diagnostic-functions'
with only `eglot-flymake-backend'.  Enabling Flymake earlier — e.g. from
`prog-mode-hook' — lets mode-local backends like `rust-ts-flymake' start
async work that races with that handoff (\"Can't find state for …\")."
    (unless (or (and (derived-mode-p 'lisp-interaction-mode)
                     (string= (buffer-name) "*scratch*"))
                (my/eglot-will-manage-p))
      (flymake-mode 1)))

  :hook (prog-mode . my/maybe-enable-flymake)

  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error))
  :custom
  (flymake-no-changes-timeout 0.5)
  ;; Inline hint for the worst diagnostic on the line (no extra packages).
  (flymake-show-diagnostics-at-end-of-line 'short))

;; Magit
;; -----------------------------------------------------------------------------
(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g l" . magit-log-all)
   ("C-c g b" . magit-blame-addition)
   ("C-c g d" . magit-dispatch)
   ("C-c g f" . magit-file-dispatch))
  :config
  (magit-auto-revert-mode t)
  (setq magit-diff-refine-hunk 'all))

;; Project
;; -----------------------------------------------------------------------------
(use-package project
  :ensure nil
  :bind ("s-p" . project-find-file)
  :config
  (add-to-list 'project-vc-extra-root-markers ".project-root"))

;;; Tree-sitter — automatic major modes (rust-ts-mode, toml-ts-mode, etc.)
;; -----------------------------------------------------------------------------
;; Deferred via after-init (not :demand): still early enough to register
;; auto-mode remaps before interactive file visits, and so missing-grammar
;; install (treesit-auto-install) can run when a ts-mode activates.
(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :custom
  ;; Prompt on first visit of a language whose grammar isn't installed yet.
  ;; Grammars land in ~/.emacs.d/tree-sitter/ as libtree-sitter-*.dylib.
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; end of init-pkgs.el
