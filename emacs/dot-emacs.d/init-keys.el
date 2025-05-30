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

;;; Global Keybindings
;; =============================================================================
(global-set-key (kbd "RET") #'newline-and-indent)   ;; Auto-indent on RET
(global-set-key (kbd "s-0") #'text-scale-adjust)    ;; Adjust text scale (font size)

;; Super-k to close current window and kill its buffer
(global-set-key (kbd "s-k") #'my/close-and-kill-this-pane)

;; Remaps
(global-set-key [remap list-buffers] #'ibuffer)          ;; Use ibuffer instead of list-buffers
(global-set-key [remap fill-paragraph] #'my/fill-or-unfill) ;; Use custom fill/unfill

;;; end of init-keys.el
