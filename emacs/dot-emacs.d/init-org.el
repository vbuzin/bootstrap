;;; init-org.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Org mode
;; =============================================================================
;; funcs and helpers
(defun my/default-capture-target ()
  (concat org-directory "_ inbox/" (format-time-string "%Y-%b") ".org"))

(defun my/agenda-and-refile-targets ()
  (directory-files-recursively org-directory "\\.org\\'" nil
                               (lambda (f) (not (string-match-p "/_ " f)))))

;; global keys
(global-set-key (kbd "\C-c o a") 'org-agenda)
(global-set-key (kbd "\C-c o c") 'org-capture)
(global-set-key (kbd "\C-c o l") 'org-store-link)

;; locations
(setq org-directory (expand-file-name "~/Documents/OM/"))
(set-register ?o `(file . ,org-directory))

(setq org-default-notes-file (my/default-capture-target)
      org-attach-directory (concat org-directory "/_ data/")
      org-archive-location (concat org-directory "/_ archive/%s_arch::"))

(setq org-agenda-files (my/agenda-and-refile-targets))
(setq org-refile-use-outline-path 'full-file-path
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;; keeping stuff secret
(with-eval-after-load 'org
  ;; encrypt headlines with the :crypt: tag
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key "0x7A9462266E8BFB56"))

(add-hook 'org-mode-hook #'visual-line-mode)

(setq org-M-RET-may-split-line nil
      org-adapt-indentation t
      org-catch-invisible-edits 'show-and-error
      org-ctrl-k-protect-subtree t
      org-current-tag-alist  '(("crypt" . ?c) ("pin" . ?p))
      org-ellipsis " ⤵"
      org-enforce-todo-checkbox-dependencies t
      org-enforce-todo-dependencies t
      org-fontify-done-headline t
      org-indent-mode t
      org-show-following-heading t
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-src-fontify-natively t
      org-startup-align-all-tables t
      org-startup-folded 'overview
      org-startup-indented t
      org-startup-indented t
      org-yank-adjusted-subtrees t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WIP(s!)" "|" "DONE(d!)")
        (sequence "ONHOLD(p@)" "|")
        (sequence "|" "CANCELED(c@)"))
      org-use-fast-todo-selection t)

(setq org-stuck-projects
      '("+project/+ONHOLD/-DONE-CANCELED" ("TODO" "WIP") nil ""))

;; Agenda
;; =============================================================================

;; Capture templates
;; =============================================================================
(defun my/org-capture-daily ()
  "Navigate to or create today's heading in the capture buffer."
  (let* ((today (format-time-string "%d-%a"))
         (pos (org-find-exact-headline-in-buffer today)))
    (if pos
        (goto-char pos)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (concat "* " today))
      (beginning-of-line))))

(setq org-capture-templates
      `(("l" "Daily Log" entry
         (file+function
          ,(my/default-capture-target)
          my/org-capture-daily)
         "** %^{What’s the context?}\n:PROPERTIES:\n:created: %U\n:END:\n%?"
         :empty-lines-after 1)))
;;; end of init-org.el
