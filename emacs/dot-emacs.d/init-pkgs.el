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
(use-package company
  :hook (prog-mode . company-mode)
  :bind (("s-/" . company-complete)
         :map company-active-map
         ([tab] . nil))
  :config
  (setq company-require-match nil)
  (setq company-show-numbers t)
  (setq company-tooltip-align-annotations t))

(use-package desktop
  :init
  (desktop-save-mode t)
  :ensure nil
  :config
  ;; Only persist frame state, not buffers
  (setq desktop-globals-to-save '((frame-parameters)))
  ;; Exclude all file-visiting buffers by matching every file name.
  (setq desktop-files-not-to-save ".*")
  ;; Do not prompt when saving the desktop:
  ;; Setting desktop-save to t causes Emacs to automatically save without asking.
  (setq desktop-save t))

(use-package doom-themes
  :init (load-theme 'doom-vibrant t)
  :config
  (let ((scale 1) (stroke 6)
        (bg (face-attribute 'mode-line :background))
        (fg (face-attribute 'default :foreground))
        (hl (face-attribute 'region :background)))
    (custom-set-faces
     `(header-line ((t (:inherit helm-header :weight bold :box (:line-width 5 :color ,bg)))))
     `(help-key-binding ((t :inherit helm-M-x-key)))
     `(Info-quoted ((t (:inherit font-lock-comment-face :slant italic))))

     `(helm-descbinds-binding ((t :inherit default)))
     `(helm-descbinds-key ((t :inherit helm-M-x-key)))
     `(helm-header ((t (:inherit mode-line))))
     `(helm-M-x-key ((t (:foreground "orange" :box nil))))))

  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-hud t)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-support-imenu t))

(use-package expand-region
  :bind ("s-=" . er/expand-region))

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
  (("C-c h b" . helm-bookmarks)
   ("C-c h g" . helm-do-grep-ag)
   ("C-c h i" . helm-semantic-or-imenu)
   ("C-c h I" . helm-imenu-in-all-buffers)
   ("C-c h m" . helm-all-mark-rings)
   ("C-c h y" . helm-show-kill-ring)

   ([remap apropos-command] . helm-apropos)
   ([remap execute-extended-command] . helm-M-x)
   ([remap find-file] . helm-find-files))
   ([remap list-buffers] . helm-buffers-list)
   ([remap occur] . helm-occur)
   ([remap switch-to-buffer] . helm-multi-files)
  :config
  (use-package helm-descbinds      :bind ([remap describe-bindings] . helm-descbinds))
  (use-package helm-describe-modes :bind ([remap describe-mode] . helm-describe-modes))

  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-grep-ag-command (concat
        "rg --color=always"
        "--smart-case"
        "--no-heading"
        "--hidden"
        "--line-number %s %s %s"))
  (setq helm-split-window-default-side 'same)

  (helm-mode t)
  (helm-adaptive-mode t))

(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down))
  :config
  ;; indent after moving
  (let ((adv (lambda (&rest ignored)
               (let ((deactivate deactivate-mark))
                 (if (region-active-p)
                     (indent-region (region-beginning) (region-end))
                   (indent-region (line-beginning-position) (line-end-position)))
                 (setq deactivate-mark deactivate)))))
    (advice-add 'move-text-up :after adv)
    (advice-add 'move-text-down :after adv)))

(use-package shrink-whitespace
  :bind
  ("s-." . shrink-whitespace))

(use-package vlf
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))

;;; end of init-pkgs.el
