;;; init-keys.el -*- lexical-binding: t; -*-

;;; Helper functions
;; =============================================================================
(defun vb/close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane
and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

(defun vb/fill-or-unfill ()
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
(global-set-key (kbd "s-k") 'vb/close-and-kill-this-pane)
(global-set-key (kbd "s-t") 'eshell)

(global-set-key (kbd "C-c w <up>") 'enlarge-window)
(global-set-key (kbd "C-c w <down>") 'shrink-window)
(global-set-key (kbd "C-c w <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-c w <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c w =") 'fit-window-to-buffer)

(global-set-key (kbd "<f5>") 'display-line-numbers-mode)
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

(global-unset-key (kbd "<f11>")) ;; now mapped to ‘C-s-f’
(global-unset-key (kbd "C-\\")) ;; making it a prefix for tmux
(global-unset-key (kbd "C-z"))

(global-set-key [remap fill-paragraph] #'vb/fill-or-unfill)

;;; registers
(set-register ?e `(file . ,user-emacs-directory))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
