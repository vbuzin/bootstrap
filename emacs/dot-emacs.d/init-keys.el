;;; init-keys.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Helper functions
;; =============================================================================
(defun my/close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane
and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun my/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'vb/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;;; Keybindings
;; =============================================================================
(global-set-key (kbd "RET") 'newline-and-indent) ;; auto-indent on RET
(global-set-key (kbd "s-0") 'text-scale-adjust) ;; font size
(global-set-key (kbd "s-k") 'my/close-and-kill-this-pane)

(global-set-key [remap fill-paragraph] #'vb/fill-or-unfill)

;;; registers
(set-register ?e `(file . ,user-emacs-directory))

;;; end of init-keys.el
