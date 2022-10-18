;;; Env and some defaults
;; =============================================================================
(if (find-font (font-spec :name "Cascadia Code"))
    (push '(font . "Cascadia Code-14") default-frame-alist))

(setenv "LANG"   "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;; fixing env on macs
(setenv "PATH"
        (concat
         (getenv "PATH")
         (concat ":" (expand-file-name "~/bin"))
         (concat ":" (expand-file-name "~/.local/bin"))
         (concat ":" (expand-file-name "~/.cargo/bin"))
         ":/usr/local/bin"
         ":/usr/local/sbin"
         ":/opt/homebrew/bin/"
         ":/Library/TeX/texbin"))

(setq exec-path (split-string (getenv "PATH") ":"))

;; remaping cmd, fn and option keys
(setq mac-command-modifier 'super
      mac-option-modifier  'meta
      ns-function-modifier 'hyper)  ; make Fn key do Hyper

(setq ns-use-proxy-icon nil
      frame-title-format " ")

(setq default-directory "~/")

(setq-default cursor-type 'hbar)
(setq-default line-spacing 2)
(setq-default fill-column 80)

;;; Place all auto-saves and backups in the temp directory
;; =============================================================================
(defconst vb:cfg-tmp-dir
  (expand-file-name
   (format "emacs%d" (user-uid)) temporary-file-directory))

(setq backup-directory-alist `((".*" . ,vb:cfg-tmp-dir))
      auto-save-file-name-transforms `((".*" ,vb:cfg-tmp-dir t))
      auto-save-list-file-prefix vb:cfg-tmp-dir)

;;; Misc
;; =============================================================================

;; protect the `*scratch*' and `*Messages*’ buffers.
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

;; if using coreutils
(when (executable-find "gls")
  (setq insert-directory-program "gls"
        dired-use-ls-dired t
        dired-listing-switches "-ADFGXclh --group-directories-first")

  (add-hook 'dired-mode-hook '(lambda () (setq mode-name "Dired"))))

;; move between windows with super+arrow
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; better scrolling
(setq scroll-margin 5
      hscroll-step 1
      scroll-conservatively 100000
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil)

(setq Man-notify-method 'bully)
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")
      ange-ftp-netrc-filename auth-sources)
(setq compilation-scroll-output t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq display-line-numbers-type 'relative)
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
(setq pop-up-frames nil)
(setq reb-re-syntax 'string)
(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq save-interprogram-paste-before-kill t)
(setq set-mark-command-repeat-pop t)
(setq shell-file-name "zsh")
(setq split-width-threshold nil)
(setq tab-stop-list (number-sequence 4 200 4))
(setq use-dialog-box nil)
(setq vc-follow-symlinks t)

(setq-default indent-tabs-mode nil)
(setq-default indicate-empty-lines t)
(setq-default tab-width 4)
(setq-default truncate-lines t)

;; mode line settings
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
(winner-mode t)                  ;; record/restor window configuration

;; enable line numbers for prog modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; use utf-8
(prefer-coding-system 'utf-8-unix)

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

;; cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Loading the rest
;; =============================================================================

;; keep custom stuff out
(setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))

;; create file if doesn’t exist
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(mapc #'(lambda (file) (load (locate-user-emacs-file file)))
      (list
       "init-custom"
       "init-pkgs"
       "init-org"
       "init-keys"
       ))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
