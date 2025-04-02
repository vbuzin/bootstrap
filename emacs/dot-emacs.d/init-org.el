;;; init-org.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Org mode
;; =============================================================================
;; global keys
(global-set-key (kbd "\C-c o a") 'org-agenda)
(global-set-key (kbd "\C-c o c") 'org-capture)
(global-set-key (kbd "\C-c o l") 'org-store-link)

;; locations
(setq org-directory (expand-file-name "~/Documents/OM/"))
;; put org path into 'o' register
(set-register ?o `(file . ,org-directory))

(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-attach-directory   (concat org-directory "/_ data/"))
(setq org-archive-location   (concat org-directory "/_ archive/%s_arch::"))

(setq org-refile-targets
      '((nil              :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(with-eval-after-load 'org
  ;; encrypt headlines
  (require 'org-crypt)
  (org-crypt-use-before-save-magic))

(setq org-adapt-indentation t)
(setq org-M-RET-may-split-line nil)
(setq org-catch-invisible-edits 'show-and-error)
(setq org-ellipsis " ⤵")
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)
(setq org-fontify-done-headline t)
(setq org-indent-mode t)
(setq org-show-following-heading t)
(setq org-startup-align-all-tables t)
(setq org-startup-folded 'overview)
(setq org-startup-indented t)
(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WIP(s!)" "|" "DONE(d!)")
        (sequence "ONHOLD(p@)" "|")
        (sequence "|" "CANCELED(c@)")))
(setq org-use-fast-todo-selection t)

(setq org-stuck-projects
      '("+project/+ONHOLD/-DONE-CANCELED" ("TODO" "WIP") nil ""))

;; Agenda
;; =============================================================================


;; Capturing
;; =============================================================================
(setq org-capture-templates
      `(("l" "Daily Log" entry
         (file+function
          (lambda () (concat org-directory "_ inbox/" (format-time-string "%Y-%b") ".org"))
          (lambda ()
            (let* ((today (format-time-string "%d-%a"))
                   (pos (org-find-exact-headline-in-buffer today)))
              (if pos
                  (goto-char pos)                  ; jump to existing heading
                (goto-char (point-max))            ; or create heading at end
                (unless (bolp) (insert "\n"))
                (insert (concat "* " today))
                (beginning-of-line))              ; ensure point is at the heading
              ;; Do NOT move to end of subtree here – Org will handle that
              )))
         "** %^{What’s the context?}\n:PROPERTIES:\n:created: %U\n:END:\n%?"
         :empty-lines-after 1)))

;;; END of init-org.el
