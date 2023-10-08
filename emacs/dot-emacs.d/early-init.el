;;; early-init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Speeding up
;; =============================================================================
(setq package-enable-at-startup nil) ;; we'll initialise packages later

(let ((orig:gc-cons-threshold gc-cons-threshold)
      (orig:file-name-handler-alist file-name-handler-alist))
  (setq gc-cons-threshold most-positive-fixnum) ;; disable GC during startup
  (setq file-name-handler-alist nil) ;; don't need during startup

  ;; restoring defaults
  (add-hook 'emacs-startup-hook
        (lambda ()
          (setq gc-cons-threshold orig:gc-cons-threshold)
          (setq file-name-handler-alist orig:file-name-handler-alist))))

;;; UI tweaks
;; =============================================================================
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)

(setq icon-title-format nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      initial-scratch-message nil)

(setq initial-frame-alist '((width . 138) (height . 48)))

(set-face-attribute 'default nil :family "Cascadia Code" :height 140)
(copy-face 'default 'variable-pitch)
(copy-face 'default 'fixed-pitch)

(setq ns-use-proxy-icon nil
      frame-title-format " ")

(setq-default cursor-type 'hbar)
(setq-default line-spacing 2)
(setq-default fill-column 80)

(setq frame-inhibit-implied-resize t)

(setq default-frame-alist
      '((ns-appearance . dark)
        (ns-transparent-titlebar . t)))


;;; Server mode
;; =============================================================================
(require 'server)
(unless (server-running-p)
  (message "Starting a server...")
  (server-start))

;;; end of early-init.el
