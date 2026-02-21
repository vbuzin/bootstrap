;;; init-keys.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Helper Functions
;; =============================================================================
(defun my/close-and-kill-this-pane ()
  "Close the current window and kill its buffer if other windows exist."
  (interactive)
  (kill-this-buffer)
  (when (> (count-windows) 1) ; Only delete window if it's not the last one
    (delete-window)))

(defun my/fill-or-unfill ()
  "Toggle paragraph filling.
Fills to `fill-column` on first call.
Unfills (sets `fill-column` to `point-max`) on subsequent immediate call."
  (interactive)
  (let ((current-fill-column fill-column)) ; Store current fill-column
    (if (eq last-command 'my/fill-or-unfill) ; Check if last command was this one
        (progn
          (setq fill-column (point-max)) ; Unfill
          (setq this-command nil)) ; Reset for next non-consecutive call
      (setq fill-column current-fill-column)) ; Ensure standard fill-column
    (call-interactively #'fill-paragraph)))

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

;;; Toggle prefix — C-c t
;; =============================================================================
(defvar-keymap my/toggle-map
  :doc "Toggle minor modes."
  "l" #'display-line-numbers-mode
  "w" #'whitespace-mode
  "v" #'visual-line-mode
  "h" #'hl-line-mode
  "f" #'visual-fill-column-mode
  "t" #'tab-bar-mode)
(keymap-global-set "C-c t" my/toggle-map)

;;; Global Keybindings
;; =============================================================================
;; Rule: bindings live with their package (:bind in use-package).
;; This file only owns: custom functions, remaps, Super/window bindings
;; for built-in commands with no package home.

;; Remaps
(define-key global-map [remap list-buffers]   #'ibuffer)
(define-key global-map [remap fill-paragraph] #'my/fill-or-unfill)

;; Return key
(keymap-global-set "RET" #'newline-and-indent)

;; Editing (custom functions / built-ins)
(keymap-global-set "s-s" #'save-buffer)
(keymap-global-set "s-z" #'undo-only)
(keymap-global-set "s-Z" #'undo-redo)
(keymap-global-set "s-d" #'my/duplicate-line-or-region)
(keymap-global-set "s-/" #'comment-or-uncomment-region)

;; Buffers & files (built-ins only; consult bindings are in init-pkgs.el)
(keymap-global-set "s-B" #'ibuffer)
(keymap-global-set "s-F" #'find-file)
(keymap-global-set "s-k" #'my/close-and-kill-this-pane)

;; Windows
(keymap-global-set "s-0" #'delete-window)
(keymap-global-set "s-1" #'delete-other-windows)
(keymap-global-set "s-2" #'split-window-below)
(keymap-global-set "s-3" #'split-window-right)

;;; end of init-keys.el
