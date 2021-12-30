;;; early-init.el -*- lexical-binding: t; -*-

;;; Speeding up
;; =============================================================================
(setq package-enable-at-startup nil)

;; Increase GC threshold during startup
(defvar vb:gc-cons-threshold-orig gc-cons-threshold)
(setq gc-cons-threshold (* 64 1024 1024)) ;; 64mb

;; Every file opened and loaded by Emacs will run through this list to
;; check for a proper handler for the file, but during startup, it
;; won’t need any of them.
(defvar vb:file-name-handler-alist-orig file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist vb:file-name-handler-alist-orig)
            (setq gc-cons-threshold vb:gc-cons-threshold-orig) ;; restoring default
            (makunbound 'vb:gc-cons-threshold-orig)
            (makunbound 'vb:file-name-handler-alist-orig)))

;; Some UI tweaks
;; =============================================================================
(push '(tool-bar-lines . 0)          default-frame-alist)
(push '(menu-bar-lines . 0)          default-frame-alist)

(push '(ns-appearance . dark)        default-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

(scroll-bar-mode -1)
(tooltip-mode    -1)

(setq icon-title-format nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; https://github.com/hlissner/doom-emacs/blob/develop/early-init.el
(setq frame-inhibit-implied-resize t)
(push '(font . "SF Mono-14") default-frame-alist)

;; Frame size and position
;; =============================================================================
;; always center on the screen
(defun vb/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame)
                            frame)
                      (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

(add-hook 'after-init-hook #'vb/frame-recenter)
(add-hook 'after-make-frame-functions #'vb/frame-recenter)

;; default size
(setq initial-frame-alist '((width . 140) (height . 58)))

;;; Emacs server
;; =============================================================================
(require 'server)
(if (not (server-running-p)) (server-start))
