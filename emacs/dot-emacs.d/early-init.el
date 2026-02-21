;;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Speeding up Startup
;; =============================================================================
(setq package-enable-at-startup nil)

;; Raise GC threshold during init, restore after
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

(setq read-process-output-max (* 4 1024 1024))

;; Defer file handler processing during startup
(let ((orig-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda () (setq file-name-handler-alist orig-file-name-handler-alist))))

;;; Minimalistic UI Tweaks
;; =============================================================================
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)

(setq frame-inhibit-implied-resize t
      frame-title-format " "
      icon-title-format nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      initial-scratch-message nil
      pop-up-frames nil
      use-dialog-box nil
      use-short-answers t)

(defun display-startup-echo-area-message () nil)

;; macOS Specific UI Settings
(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil
        ns-pop-up-frames nil
        default-frame-alist
        '((top    . 34)
          (left   . 702)
          (width  . 88)
          (height . 54)
          (ns-appearance . dark)
          (ns-transparent-titlebar . t))))

(set-frame-parameter nil 'internal-border-width 10)

;; Font Configuration
(set-face-attribute 'default nil :family "Monaspace Argon" :height 130)
(copy-face 'default 'fixed-pitch)
(copy-face 'default 'fixed-pitch-serif)
(copy-face 'default 'variable-pitch)

;; Default Fill Column
(setq-default fill-column 80)

;;; end of early-init.el
