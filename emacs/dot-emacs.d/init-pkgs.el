;;; init-pkgs.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Package System Configuration
;; =============================================================================
;; Set directory for packages
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(require 'package)

;; Define package archives
(setq package-archives
      '(("gnu"     . "https://elpa.gnu.org/packages/")
        ("melpa"   . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa"   . 10)
        ("gnu"     . 0)))

;; Initialize the package system
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ;; Update package contents before installing
  (package-install 'use-package))

;; Configure `use-package` defaults
(setq use-package-always-defer t         ;; Defer loading of packages
      use-package-always-ensure t        ;; Ensure packages are installed
      use-package-enable-imenu-support t) ;; Enable imenu support for use-package blocks

(eval-when-compile (require 'use-package)) ;; Ensure use-package macros are available at compile-time
(eval-when-compile (require 'bind-key))   ;; Often used by use-package for :bind

;;; Core Packages
;; =============================================================================

;; Completion System (Vertico, Consult, Marginalia, Orderless)
;; -----------------------------------------------------------------------------
(use-package vertico
  :init
  (vertico-mode) ;; Enable Vertico globally
  :custom
  (vertico-count 12) ;; Show more candidates
  (vertico-cycle t)) ;; Enable cycling for `vertico-next/previous'

(use-package consult
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  :bind
  ("s-; b" . consult-buffer)
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
  :config
  (setq consult-narrow-key "<"))

(use-package marginalia
  :init
  (marginalia-mode)) ;; Enable annotations in the minibuffer

(use-package orderless
  :custom
  (completion-styles '(orderless basic)) ;; Use orderless completion style
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Dired (Directory Editor)
;; -----------------------------------------------------------------------------
(use-package dired
  :ensure nil ;; Built-in package
  :hook
  (dired-mode . (lambda () (require 'dired-x)))
  :config
    (setq dired-dwim-target t               ;; Do What I Mean for target window
          dired-reuse-buffer t              ;; Reuse dired buffer for visited directories
          dired-recursive-copies 'top       ;; Ask before recursive copies
          dired-recursive-deletes 'top      ;; Ask before recursive deletes
          ls-lisp-dirs-first t              ;; Directories first, then the rest
          ls-lisp-use-insert-directory-program nil
          wdired-allow-to-change-permissions t)) ;; Allow to change file permission bits

;; Modeline
;; -----------------------------------------------------------------------------
(use-package doom-modeline
  :init (doom-modeline-mode t) ;; Enable Doom modeline globally
  :config
  (setq doom-modeline-height 24        ;; Set modeline height
        doom-modeline-hud t            ;; Enable Heads Up Display features
        doom-modeline-icon nil         ;; Disable icons in modeline (personal preference)
        doom-modeline-support-imenu t)) ;; Enable imenu support

;; Themes
;; -----------------------------------------------------------------------------
(use-package doom-themes
  :init (load-theme 'doom-one t) ;; Load 'doom-one' theme on startup
  :config
  ;; Custom face adjustments for consistency with the theme
  (custom-set-faces
   `(help-key-binding ((t :box nil)))
   `(fringe ((t (:inherit default :foreground ,(face-attribute 'default :foreground)))))
   `(line-number-current-line ((t (:inherit default :foreground ,(face-attribute 'default :foreground) :weight normal))))
   `(Info-quoted ((t (:inherit font-lock-comment-face :slant italic)))))
  (doom-themes-org-config))

;; Utility Packages
;; -----------------------------------------------------------------------------
(use-package anzu
  :init
  (global-anzu-mode t)
  :bind
  (([remap query-replace] . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)))

(use-package avy
  :bind
  ("s-`" . avy-goto-char-2))

(use-package duplicate-thing
  :bind ("s-d" . duplicate-thing)) ;; Duplicate line or region

(use-package expand-region
  :bind ("s-=" . er/expand-region)) ;; Intelligently expand selection

(use-package fringe-current-line
  :demand t ;; Load eagerly as it's a global mode
  :config
  (global-fringe-current-line-mode t)) ;; Highlight current line in the fringe

(use-package move-text
  :bind (("M-s-<up>" . move-text-up)     ;; Move text block up
         ("M-s-<down>" . move-text-down)) ;; Move text block down
  :config
  ;; Automatically indent text after moving
  (dolist (command '(move-text-up move-text-down))
    (advice-add command :after
                (lambda (&rest _)
                  (let ((deactivate deactivate-mark)) ; Preserve mark deactivation state
                    (if (region-active-p)
                        (indent-region (region-beginning) (region-end))
                      (indent-region (line-beginning-position) (line-end-position)))
                    (setq deactivate-mark deactivate))))))

(use-package shrink-whitespace
  :bind ("C-c ." . shrink-whitespace)) ;; Intelligently delete whitespace

(use-package visual-fill-column
  :hook
  (org-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 80))

(use-package vlf ;; View Large Files
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask)) ;; Automatically use VLF for large files

(use-package undo-tree
  :init
  (global-undo-tree-mode t)
  :config
  (setq undo-tree-history-directory-alist `(("." . ,my:emacs-tmp-dir))))

;; Coding
;; -----------------------------------------------------------------------------
(use-package company
  :hook (prog-mode . company-mode)
  :bind (("s-/" . company-complete)
         :map company-active-map
         ([tab] . nil))
  :config
  (setq company-require-match nil)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package lsp-mode
  :commands lsp
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  ;; eslint
  (setq lsp-eslint-auto-fix-on-save t
        lsp-eslint-format t
        lsp-eslint-validate '("javascript" "javascriptreact" "typescript" "typescriptreact")
        lsp-format-buffer-on-save t
        ;; disable ts-ls formatting
        lsp-javascript-format-enable nil
        lsp-typescript-format-enable nil)

  (setq lsp-headerline-breadcrumb-enable nil))

(use-package lsp-tailwindcss
  :after lsp-mode
  :init (setq lsp-tailwindcss-add-on-mode t))

(use-package magit
  :bind
  (("C-c ms" . magit-status)
   ("C-c ml" . magit-log-all)
   ("C-c mb" . magit-blame-addition)
   ("C-c md" . magit-dispatch)
   ("C-c mf" . magit-file-popup))
  :config
  (use-package transient
    :pin melpa)

  (magit-auto-revert-mode t)
  (setq magit-diff-refine-hunk 'all))

(use-package project
  :ensure nil
  :config
  (add-to-list 'project-vc-extra-root-markers ".project-root"))

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;;; end of init-pkgs.el
