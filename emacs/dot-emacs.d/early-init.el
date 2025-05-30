;;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Speeding up Startup
;; =============================================================================
(setq package-enable-at-startup nil) ;; Initialize packages manually later

;; More meaningful GC threshold
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Defer file handler processing during startup
(let ((orig-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda () (setq file-name-handler-alist orig-file-name-handler-alist))))

;;; Minimalistic UI Tweaks
;; =============================================================================
(scroll-bar-mode -1)    ;; Disable scroll bars
(tool-bar-mode   -1)    ;; Disable tool bar
(tooltip-mode    -1)    ;; Disable tooltips

(setq frame-inhibit-implied-resize t      ;; Prevent Emacs from auto-resizing frames
      frame-title-format " "              ;; Minimal frame title
      icon-title-format nil               ;; No icon title
      inhibit-splash-screen t             ;; Disable splash screen
      inhibit-startup-screen t            ;; Disable startup screen (GNU screen)
      initial-scratch-message nil         ;; No message in *scratch* buffer
      pop-up-frames nil                   ;; Prevent new frames for pop-ups (use windows instead)
      use-dialog-box nil                  ;; Use text-based dialogs instead of GUI
      use-short-answers t)                ;; Use 'y' or 'n' instead of 'yes' or 'no'

(defun display-startup-echo-area-message () nil)

;; macOS Specific UI Settings
(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil          ;; Don't use proxy icon in title bar on macOS
        ns-pop-up-frames nil           ;; Matches pop-up-frames behavior for macOS
        default-frame-alist
        '((top    . 34)
          (left   . 702)
          (width  . 88)
          (height . 54)
          (ns-appearance . dark)
          (ns-transparent-titlebar . t))))

(set-frame-parameter nil 'internal-border-width 10) ;; Add some padding

;; Font Configuration
;; Consider a fallback font if "SF Mono" isn't universally available.
(set-face-attribute 'default nil :family "SF Mono" :height 130)
(copy-face 'default 'fixed-pitch)
(copy-face 'default 'fixed-pitch-serif)
(copy-face 'default 'variable-pitch)

;; Default Fill Column
(setq-default fill-column 80)             ;; Keep fill-column at 80 for buffers that use it

;;; end of early-init.el
