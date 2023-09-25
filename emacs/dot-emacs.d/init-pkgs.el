;;; init-pkgs.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Configure packages
;; =============================================================================
(setq package-user-dir (expand-file-name "packages" user-emacs-directory))
(require 'package)

(setq package-archives
      '(("gnu"     . "https://elpa.gnu.org/packages/")
        ("melpa"   . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu"     . 10)
        ("melpa"   . 0)))

;; initialize package
(package-initialize)

;; bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-defer t
      use-package-always-ensure t
      use-package-enable-imenu-support t)

(eval-when-compile (require 'use-package))
(require 'bind-key)

;;; Core packages
;; =============================================================================
(use-package avy
  :bind
  (("s-j" . avy-goto-char)
   ("s-J" . avy-goto-char-2)))

(use-package anzu
  :hook (after-init . global-anzu-mode)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

(use-package beacon
  :init
  (beacon-mode 1)
  :bind ("C-` b" . beacon-blink))

(use-package company
  :hook (after-init . global-company-mode)
  :bind (("s-/" . company-complete)
         :map company-active-map
         ([tab] . nil))
  :config
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-idle-delay 0)
  (setq company-require-match nil)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t))

(use-package dired
  :ensure nil
  :config
  (require'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-continuous-word-count-modes
        '(markdown-mode gfm-mode org-mode))
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-height 24)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-persp-name nil))

(use-package doom-themes
  :init
  (load-theme 'doom-one t)
  :config
  (let ((scale 0.9) (stroke 7)
        (bg (face-attribute 'mode-line :background))
        (fg (face-attribute 'default :foreground)))
    (custom-set-faces
     `(fringe ((t (:inherit default :foreground ,fg))))
     `(header-line ((t (:height ,scale :box (:line-width ,stroke :color ,bg)))))
     `(line-number-current-line ((t (:inherit default :weight normal))))
     `(mode-line ((t (:height ,scale))))
     `(helm-grep-finish ((t (:inherit mode-line :box (:line-width ,stroke :color ,bg)))))
     `(helm-header ((t (:height ,scale :box (:line-width ,stroke :color ,bg)))))
     `(helm-source-header ((t (:background nil :weight semi-bold))))))

  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic t)
  (doom-themes-org-config))

(use-package duplicate-thing
  :bind ("s-d" . duplicate-thing))

(use-package expand-region
  :bind (("s-=" . er/expand-region)
         ("s--" . er/contract-region)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-display-errors-function
        #'flycheck-display-error-messages-unless-error-list)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package fringe-current-line
  :demand t
  :config
  (global-fringe-current-line-mode t))

(use-package helm
  :hook (after-init . helm-mode)
  :bind
  (("C-h a" . helm-apropos)
   ("C-h I" . helm-info)

   ("C-` c" . helm-resume)
   ("C-` b" . helm-filtered-bookmarks)
   ("C-` f" . helm-find)
   ("C-` g" . helm-do-grep-ag)

   ("s-f" . helm-find-files)
   ("s-b" . helm-mini)
   ("s-i" . helm-semantic-or-imenu)
   ("s-m" . helm-all-mark-rings)
   ("s-o" . helm-occur)
   ("s-y" . helm-show-kill-ring)

   ([remap execute-extended-command] . helm-M-x)
   ([remap find-file] . helm-find-files))

  :config
  (use-package helm-descbinds      :bind ("C-h b" . helm-descbinds))
  (use-package helm-describe-modes :bind ("C-h m" . helm-describe-modes))

  (setq completion-styles '(flex))
  (setq helm-candidate-number-limit 100)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-grep-ag-command "rg --color=always --colors \
'match:fg:magenta' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-show-completion-display-function 'helm-default-display-buffer)
  (setq helm-split-window-default-side 'same)

  (helm-mode t)
  (helm-adaptive-mode t))

(use-package helpful
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-key]      . helpful-key)
   ([remap describe-symbol]   . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ("C-h ." . helpful-at-point)))

(use-package indent-guide
  :hook (prog-mode . indent-guide-mode))

(use-package magit
  :config
  (magit-auto-revert-mode t)
  (setq magit-diff-refine-hunk 'all)
  :bind
  (("C-` ms" . magit-status)
   ("C-` ml" . magit-log-all)
   ("C-` mb" . magit-blame-addition)
   ("C-` md" . magit-dispatch)
   ("C-` mf" . magit-file-popup)))

(use-package move-text
  :bind (("M-S-<up>"   . move-text-up)
         ("M-S-<down>" . move-text-down)))

(use-package project)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  ((prog-mode . rainbow-mode)))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 100)

  (add-to-list 'recentf-exclude "\\.gpg\\'")
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\|ftp\\)?:")
  (add-to-list 'recentf-exclude (file-name-directory (car (last load-path)))))

(use-package shackle
  :hook (after-init . shackle-mode)
  :config
  (with-eval-after-load 'org
    ;; HACK: compatibility issue with `org-switch-to-buffer-other-window'
    (advice-add #'org-switch-to-buffer-other-window
                :override #'switch-to-buffer-other-window)

    ;; tags buffer is too small otherwise
    (advice-add #'org-fit-window-to-buffer
                :around #'(lambda (orig-fun &rest args)
                            (if (string= (buffer-name) " *Org tags*")
                                (delete-other-windows)
                              (apply orig-fun args)))))

  (setq shackle-default-rule nil)
  (setq shackle-rules
        '(("CAPTURE-.*+\\.org$?\\|*Org Select*" :regexp t :same t)
          ("*Capture*" :same t)
          (completion-list-mode :same t)
          (Info-mode :same t)
          (help-mode :same t)
          (helpful-mode :same t))))

(use-package shrink-whitespace
  :bind
  ("C-` ." . shrink-whitespace))

(use-package vlf
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))

(use-package which-key
  :demand t
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

(use-package yasnippet
  :defer 1
  :config
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  (yas-global-mode)
  (yas-reload-all))

;;; end of init-pkgs.el
