;;; init-org.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Org mode
;; =============================================================================
;; global keys
(global-set-key (kbd "\C-c oa") 'org-agenda)
(global-set-key (kbd "\C-c oc") 'org-capture)
(global-set-key (kbd "\C-c ol") 'org-store-link)

;; locations
(setq org-directory (expand-file-name "~/Documents/Org/"))
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(setq org-attach-directory   (concat org-directory "/data/"))
(setq org-archive-location   (concat org-directory "/archive/%s_arch::"))

(set-register ?o `(file . ,org-directory))

;; logging
(setq org-log-into-drawer t)
(setq org-log-done 'note)
(setq org-log-redeadline 'note)
(setq org-log-refile 'note)
(setq org-log-reschedule 'time)

;; refile targets
(setq org-refile-targets
      '((nil              :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup.
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; save clock history across Emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  ;; keybindings
  (define-key org-mode-map (kbd "\C-c op") 'org-property-action)

  ;; encrypt headlines
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))

  (setq org-M-RET-may-split-line nil)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-clock-idle-time 5)
  (setq org-deadline-warning-days 2)
  (setq org-display-inline-images nil)
  (setq org-ellipsis " ⤵")
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-export-with-section-numbers nil)
  (setq org-fontify-done-headline t)
  (setq org-goto-auto-isearch t)
  (setq org-hide-emphasis-markers nil)
  (setq org-image-actual-width nil)
  (setq org-indent-mode t)
  (setq org-list-allow-alphabetical t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-show-following-heading t)
  (setq org-show-hierarchy-above t)
  (setq org-special-ctrl-a/e t)
  (setq org-startup-align-all-tables t)
  (setq org-startup-folded 'overview)
  (setq org-startup-indented t)
  (setq org-tags-column -90)

  (setq org-tag-alist
        '((:startgroup . "type")
          ("pers"      . ?p)
          ("work"      . ?j)
          (:endgroup)
          ("project"   . ?P)
          ("crypt"     . ?c)))

  (setq org-global-properties '(("Effort_ALL" . "XS S M L XL")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WIP(s!)" "|" "DONE(d!)")
          (sequence "ONHOLD(p@)" "|")
          (sequence "|" "CANCELED(c@)")))
  (setq org-use-fast-todo-selection t)

  (setq org-stuck-projects
        '("+project/+ONHOLD/-DONE-CANCELED" ("TODO" "WIP") nil ""))

  (add-hook 'org-mode-hook #'(lambda () (auto-fill-mode  t)))

;; Agenda
;; =============================================================================
(setq org-agenda-dim-blocked-tasks t)
(setq org-agenda-include-diary nil) ;; use diary.org
(setq org-agenda-show-future-repeats nil)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-span 'week)
(setq org-agenda-todo-ignore-deadlines 'far)
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-agenda-window-setup 'only-window)

(setq org-agenda-files (list
                        (concat org-directory "inbox.org")
                        (concat org-directory "projects")))

;; Capturing
;; =============================================================================
(setq org-capture-templates
      `(("t" "Todo" entry
         (file+headline org-default-notes-file "Tasks")
         "* TODO %^{Title} %^G\n%?")

        ("n" "Note" entry
         (file+headline org-default-notes-file "Notes")
         "* %^{Title}\n:LOGBOOK: \n- Added: %U \n:END:\n%?")

        ("d" "Daily Log" entry
         (file+olp+datetree ,(concat org-directory "/dailylog.org.gpg"))
         "**** %U %? \n" :tree-type week)

        ("s" "Strategic Journal" entry
         (file+olp+datetree ,(concat org-directory "/strategic.org.gpg"))
         "**** %U %? \n" :tree-type week))))

;;; END of init-org.el
