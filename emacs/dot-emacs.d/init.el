;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Env
;; =============================================================================
(set-language-environment "UTF-8")
(setq default-input-method nil) ;; the previous sets this one which we don't want

(setq default-directory "~/")

;; place all auto-saves and backups in the temp directory
(defconst my:cfg-tmp-dir
  (expand-file-name
   (format "emacs%d" (user-uid)) temporary-file-directory))

(setq backup-directory-alist `((".*" . ,my:cfg-tmp-dir))
      auto-save-file-name-transforms `((".*" ,my:cfg-tmp-dir t))
      auto-save-list-file-prefix my:cfg-tmp-dir)

;;; Misc
;; =============================================================================
;; remaping cmd, fn and option keys
(setq mac-command-modifier 'super
      mac-option-modifier  'meta)

;; protect the `*scratch*' and `*Messages*’ buffers.
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

;; move between windows with super+arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; better scrolling
(setq Man-notify-method 'bully)
(setq compilation-scroll-output t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq dired-listing-switches "-alF")
(setq display-time-default-load-average nil)
(setq display-time-format " %I:%M%p ")
(setq echo-keystrokes 0.02)
(setq epa-pinentry-mode 'loopback)
(setq history-delete-duplicates t)
(setq kill-do-not-save-duplicates t)
(setq line-move-visual t)
(setq make-backup-files nil)
(setq mode-line-percent-position '(-3 "%o"))
(setq ns-pop-up-frames nil)
(setq pixel-scroll-precision-mode t)
(setq pop-up-frames nil)
(setq reb-re-syntax 'string)
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq save-interprogram-paste-before-kill t)
(setq scroll-conservatively 100000)
(setq scroll-margin 5)
(setq scroll-preserve-screen-position t)
(setq set-mark-command-repeat-pop t)
(setq shell-file-name "zsh")
(setq split-width-threshold nil)
(setq tab-stop-list (number-sequence 4 200 4))
(setq use-dialog-box nil)
(setq vc-follow-symlinks t)
(setq which-key-side-window-location 'right)

(setq-default fringe-indicator-alist nil) ;; disable fringe bitmap
(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq-default tab-width 4)
(setq-default truncate-lines t)

;; mode line settings
(delete-selection-mode t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; some useful modes
(delete-selection-mode t)        ;; delete the selection with a keypress
(electric-indent-mode t)         ;; automatically reindent as you type
(electric-pair-mode t)           ;; automatic parens pairing
(electric-quote-mode t)          ;; on-the-fly requoting
(fringe-mode nil)                ;; enable fringes
(global-font-lock-mode t)        ;; font-lock mode in all buffers.
(global-prettify-symbols-mode t) ;; replace lambda with λ
(save-place-mode t)              ;; open a file at the last place visited
(show-paren-mode t)              ;; visualise matching parens
(which-function-mode t)          ;; displays current function name in the mode line
(which-key-mode t)               ;; show keybinding for the mode after 1 sec

;; calendar
(setq calendar-date-style 'european)
(setq calendar-week-start-day 1)

(setq calendar-today-marker 'calendar-today)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

;; show week numbers
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'isearch))

(setq calendar-intermonth-header
      (propertize "wk"
                  'font-lock-face 'isearch))

;; don't babysit me
(mapc (lambda (x) (put x 'disabled nil))
      '(downcase-region
        erase-buffer
        narrow-to-region
        upcase-region))

;; line numbers for prog modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; display help full size
(add-to-list 'display-buffer-alist '("*Help*" display-buffer-same-window))

;; cleanup on save
(setq whitespace-style '(trailing missing-newline-at-eof))
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; Loading the rest
;; =============================================================================
;; keep custom stuff out
(setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))

;; create file if doesn’t exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; put config path into 'e' register
(set-register ?e `(file . ,user-emacs-directory))
(mapc #'(lambda (file) (load (locate-user-emacs-file file)))
      (list
       "init-custom"
       "init-pkgs"
       "init-org"
       "init-keys"
       ))
;;; end of init.el
