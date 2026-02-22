;;; init-pkgs.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Package System Configuration
;; =============================================================================
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(require 'package)
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
      use-package-always-ensure t
      use-package-enable-imenu-support t)

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
           ("F#"    (mode . fsharp-mode))
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
(use-package doom-themes
  :demand t                          ; ← override global defer
  :config                            ; ← moved (load-theme) from :init to :config
  (load-theme 'doom-tokyo-night t)
  (custom-set-faces
   `(help-key-binding ((t :box nil)))
   `(fringe ((t (:inherit default :foreground ,(face-attribute 'default :foreground)))))
   `(line-number-current-line ((t (:inherit default :foreground ,(face-attribute 'default :foreground) :weight normal))))
   `(Info-quoted ((t (:inherit font-lock-comment-face :slant italic)))))
  (doom-themes-org-config))

;; Utility Packages
;; -----------------------------------------------------------------------------
(use-package avy
  :bind (("s-`" . avy-goto-char-2)
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

;; Eglot (built-in LSP client) — replaces lsp-mode
;; -----------------------------------------------------------------------------
(use-package eglot
  :ensure nil
  :hook ((fsharp-mode . eglot-ensure))
  :bind (:map eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format-buffer)
         ("C-c l d" . eldoc-print-current-symbol-info)
         ("C-c l i" . eglot-find-implementation)
         ("C-c l t" . eglot-find-typeDefinition))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format full))
  (eglot-events-buffer-size 0)
  (eglot-send-changes-idle-time 0.5)
  (eglot-extend-to-xref t)
  :config
  ;; Performance: don't log jsonrpc events
  (fset #'jsonrpc--log-event #'ignore)

  (add-to-list 'eglot-server-programs
               '(fsharp-mode . ("fsautocomplete" "--adaptive-lsp-server-enabled")))

  ;; Format F# on save
  (defun my/eglot-format-on-save ()
    "Format buffer via eglot before saving, if eglot is active."
    (when (bound-and-true-p eglot--managed-mode)
      (eglot-format-buffer)))
  (add-hook 'before-save-hook #'my/eglot-format-on-save)

  (defun my/eglot-fsharp-cleanup-doc (orig-fun &rest args)
    "Simplify F# docs by removing VS Code links and markdown code markers."
    (let ((contents (nth 0 args)))
      (let* ((is-plist (listp contents))
             (str (if is-plist (plist-get contents :value) contents)))
        (when (stringp str)
          ;; 1. Remove the VS Code 'command:' links
          (setq str (replace-regexp-in-string "<a href=['\"]command:fsharp.showDocumentation\\?.*?['\"]>.*?</a>" "" str))

          ;; 2. Remove ```fsharp and ``` but KEEP the content
          (setq str (replace-regexp-in-string "```\\(?:fsharp\\)?" "" str))
          (setq str (replace-regexp-in-string "```" "" str))

          ;; 3. Clean up extra newlines for a tighter look
          (setq str (replace-regexp-in-string "\n\n+" "\n\n" str))

          ;; 4. Re-package
          (if is-plist
              (setq contents (list :kind "plaintext" :value str))
            (setq contents str)))
        (apply orig-fun (cons contents (cdr args))))))

  (advice-add 'eglot--format-markup :around #'my/eglot-fsharp-cleanup-doc))

;; Flymake (built-in) — replaces flycheck; eglot feeds diagnostics into it
;; -----------------------------------------------------------------------------
(use-package flymake
  :ensure nil
  :init
  (defun my/maybe-enable-flymake ()
    "Enable Flymake in programming buffers, except *scratch*."
    (unless (and (derived-mode-p 'lisp-interaction-mode)
                 (string= (buffer-name) "*scratch*"))
      (flymake-mode 1)))

  :hook (prog-mode . my/maybe-enable-flymake)

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

(use-package eglot-fsharp
  :after (eglot fsharp-mode)
  :demand t)

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

;;; Tree-sitter — reproducible F# support (core Emacs, zero extra deps for install)
;; -----------------------------------------------------------------------------
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (let ((fsharp-recipe
         (make-treesit-auto-recipe
          :lang 'fsharp
          :ts-mode 'fsharp-ts-mode
          :remap 'fsharp-mode
          :url "https://github.com/ionide/tree-sitter-fsharp"
          :revision "main"
          :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list fsharp-recipe))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; end of init-pkgs.el
