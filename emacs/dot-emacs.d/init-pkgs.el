;;; init-pkgs.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Package System Configuration
;; =============================================================================
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(require 'package)

(setq package-archives
      '(("gnu"     . "https://elpa.gnu.org/packages/")
        ("nongnu"  . "https://elpa.nongnu.org/nongnu/")
        ("melpa"   . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu"     . 10)
        ("nongnu"  .  5)
        ("melpa"   .  0)))

;; use-package is built-in since Emacs 29 — no bootstrap needed
(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-enable-imenu-support t)

;;; Core Packages
;; =============================================================================

;; Minibuffer Completion (Vertico + Consult + Marginalia + Orderless)
;; -----------------------------------------------------------------------------
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-count 12)
  (vertico-cycle t))

(use-package consult
  :bind
  (("s-; b" . consult-buffer)
   ("s-; c" . consult-mode-command)
   ("s-; f" . consult-fd)
   ("s-; g" . consult-ripgrep)
   ("s-; i" . consult-imenu)
   ("s-; I" . consult-imenu-multi)
   ("s-; l" . consult-line)
   ("s-; L" . consult-line-multi)
   ("s-; o" . consult-outline)
   ("s-; m" . consult-mark)
   ("s-; M" . consult-global-mark)
   ("s-; r" . consult-register-store)
   ("s-; y" . consult-yank-pop)
   ("s-; e" . consult-flymake))
  :config
  (setq consult-narrow-key "<"))

(use-package marginalia
  :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot (styles orderless basic)))))

;; In-buffer Completion (Corfu + Cape) — replaces company-mode
;; -----------------------------------------------------------------------------
(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  :bind (:map corfu-map
         ("s-/" . corfu-complete)
         ("RET" . nil))
  :config
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if completion is expected."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

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

;; Modeline
;; -----------------------------------------------------------------------------
(use-package doom-modeline
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-height 24
        doom-modeline-hud t
        doom-modeline-icon nil
        doom-modeline-support-imenu t))

;; Themes
;; -----------------------------------------------------------------------------
(use-package doom-themes
  :init (load-theme 'doom-one t)
  :config
  (custom-set-faces
   `(help-key-binding ((t :box nil)))
   `(fringe ((t (:inherit default :foreground ,(face-attribute 'default :foreground)))))
   `(line-number-current-line ((t (:inherit default :foreground ,(face-attribute 'default :foreground) :weight normal))))
   `(Info-quoted ((t (:inherit font-lock-comment-face :slant italic)))))
  (doom-themes-org-config))

;; Utility Packages
;; -----------------------------------------------------------------------------
(use-package avy
  :bind ("s-`" . avy-goto-char-2))

(use-package expand-region
  :bind ("s-=" . er/expand-region))

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

;; Duplicate line — simple built-in replacement for duplicate-thing
(defun my/duplicate-line-or-region ()
  "Duplicate the current line or active region."
  (interactive)
  (if (region-active-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
        (goto-char (region-end))
        (insert text))
    (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
      (end-of-line)
      (newline)
      (insert line))))
(global-set-key (kbd "s-d") #'my/duplicate-line-or-region)

;;; Development
;; =============================================================================

;; Eglot (built-in LSP client) — replaces lsp-mode
;; -----------------------------------------------------------------------------
(use-package eglot
  :ensure nil
  :hook ((fsharp-mode . eglot-ensure))
  :bind (:map eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format-buffer)
         ("C-c l d" . eldoc)
         ("C-c l i" . eglot-find-implementation)
         ("C-c l t" . eglot-find-typeDefinition))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  :config
  ;; F# — fsautocomplete
  (add-to-list 'eglot-server-programs
               '(fsharp-mode . ("fsautocomplete" "--adaptive-lsp-server-enabled")))

  ;; Performance: don't log jsonrpc events
  (fset #'jsonrpc--log-event #'ignore)

  ;; Format F# on save
  (defun my/eglot-format-on-save ()
    "Format buffer via eglot before saving, if eglot is active."
    (when (bound-and-true-p eglot--managed-mode)
      (eglot-format-buffer)))
  (add-hook 'before-save-hook #'my/eglot-format-on-save))

;; Flymake (built-in) — replaces flycheck; eglot feeds diagnostics into it
;; -----------------------------------------------------------------------------
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error))
  :custom
  (flymake-no-changes-timeout 0.5))

;; F# — top-notch configuration
;; -----------------------------------------------------------------------------
(use-package fsharp-mode
  :mode ("\\.fs[iylx]?\\'" . fsharp-mode)
  :config
  (setq inferior-fsharp-program nil)

  (defun my/fsharp-prettify ()
    "Set up prettify-symbols for F#."
    (setq-local prettify-symbols-alist
                '(("->"  . ?→)
                  ("=>"  . ?⇒)
                  ("|>"  . ?▷)
                  ("<|"  . ?◁)
                  (">>"  . ?≫)
                  ("<<"  . ?≪)
                  ("<>"  . ?≠)
                  (">="  . ?≥)
                  ("<="  . ?≤)
                  ("fun" . ?λ)))
    (prettify-symbols-mode 1))
  (add-hook 'fsharp-mode-hook #'my/fsharp-prettify)

  (add-hook 'fsharp-mode-hook #'subword-mode)
  (add-hook 'fsharp-mode-hook #'eldoc-mode))

;; Lisp — Sly for Common Lisp, Paredit for structural editing
;; -----------------------------------------------------------------------------
(use-package sly
  :commands sly
  :config
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "4096"))
          (ccl  ("ccl64")))
        sly-default-lisp 'sbcl
        sly-net-coding-system 'utf-8-unix)
  (setq sly-complete-symbol-function 'sly-flex-completions))

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode       . paredit-mode)
         (sly-mrepl-mode  . paredit-mode)
         (scheme-mode     . paredit-mode))
  :bind (:map paredit-mode-map
         ("M-s" . nil)))

;; Eldoc — built-in, just tune it
(setq eldoc-echo-area-use-multiline-p nil
      eldoc-idle-delay 0.25)

;; Magit
;; -----------------------------------------------------------------------------
(use-package magit
  :bind
  (("C-c ms" . magit-status)
   ("C-c ml" . magit-log-all)
   ("C-c mb" . magit-blame-addition)
   ("C-c md" . magit-dispatch)
   ("C-c mf" . magit-file-dispatch))
  :config
  (magit-auto-revert-mode t)
  (setq magit-diff-refine-hunk 'all))

;; Project
;; -----------------------------------------------------------------------------
(use-package project
  :ensure nil
  :config
  (add-to-list 'project-vc-extra-root-markers ".project-root"))

;; Tree-sitter (built-in) — only for what we actually use
;; -----------------------------------------------------------------------------
(when (treesit-available-p)
  (setq treesit-font-lock-level 4))

;;; end of init-pkgs.el
