;;; init-org.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Org mode
;; =============================================================================
;; global keys
(global-set-key (kbd "\C-c o a") 'org-agenda)
(global-set-key (kbd "\C-c o c") 'org-capture)
(global-set-key (kbd "\C-c o l") 'org-store-link)

;; locations
(setq org-directory (expand-file-name "~/Documents/OM/"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-attach-directory   (concat org-directory "/data/"))
(setq org-archive-location   (concat org-directory "/archive/%s_arch::"))

(with-eval-after-load 'org
  ;; encrypt headlines
  (require 'org-crypt)
  (org-crypt-use-before-save-magic))

(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-M-RET-may-split-line nil)
(setq org-catch-invisible-edits 'show-and-error)

(setq org-ellipsis " ⤵")
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)

(setq org-fontify-done-headline t)
(setq org-startup-align-all-tables t)
(setq org-startup-folded 'overview)
(setq org-startup-indented t)

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
(defun my/org-daily-log-entry ()
  "Return a marker in the current Org buffer where today’s log entry should
be inserted. If a Level‑1 headline matching today’s date (formatted as
\"dd-Mmm\", e.g. \"31-Mar\") exists, move into its body and ensure
there’s an indented blank line (so that captured entries become its
children).  If no such header exists, create one at the end of the
file (with an indented blank line as its initial body) and return a
marker at that spot."
  (let ((today (format-time-string "%d-%b")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (concat "^\\* " (regexp-quote today) "$") nil t)
          (progn
            ;; Found today's header.
            (org-back-to-heading t)
            (forward-line 1)
            ;; If at end of file or if the next headline begins immediately, insert an indented blank line.
            (if (or (eobp) (looking-at "^\\*"))
                (insert "\n  ")
              ;; If there is already a body line but it isn’t indented, fix it.
              (unless (looking-at "^  ")
                (delete-region (point) (line-end-position))
                (insert "  ")))
            (point-marker))
        ;; If not found, append a new header for today.
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " today "\n")
        (insert "  ")  ; insert a blank indented line so that capture will be inside the header’s body
        (point-marker)))))

(setq org-capture-templates
      '(("d" "Daily Log" entry
         (file+function
          (lambda () (concat org-directory "/_ inbox/" (format-time-string "%Y-%b") ".org"))
          my/org-daily-log-entry)
         "** %<%R> %?")))

;; (setq org-capture-templates
;;       '(("l" "Daily Log" entry
;;          (file+function
;;           (lambda ()
;;             (concat org-directory "/_ inbox/" (format-time-string "%Y-%b") ".org"))
;;           my/org-capture-date-and-time-entry)
;;          " %?")))

;;; END of init-org.el
