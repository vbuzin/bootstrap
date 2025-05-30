;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Environment Setup
;; =============================================================================
(set-language-environment "UTF-8")
(setq default-input-method nil)    ;; Don't set a default input method from language env

(setq default-directory "~/")      ;; Start in home directory

;; Centralize temporary files (backups, auto-saves)
(defconst my:emacs-tmp-dir
  (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory)
  "Directory for Emacs temporary files like backups and auto-saves.")
(make-directory my:emacs-tmp-dir t) ; Ensure directory exists

(setq backup-directory-alist `((".*" . ,my:emacs-tmp-dir))
      auto-save-file-name-transforms `((".*" ,my:emacs-tmp-dir t))
      auto-save-list-file-prefix my:emacs-tmp-dir
      create-lockfiles nil) ; Avoid littering directories with lockfiles

;; macOS Specific Environment
(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil          ;; Don't use proxy icon in title bar on macOS
        ns-pop-up-frames nil           ;; Matches pop-up-frames behavior for macOS
        default-frame-alist
        (append '((ns-appearance . dark) ;; Use dark mode on macOS
                  (ns-transparent-titlebar . t))
                default-frame-alist))

  ;; Add Homebrew executables to Emacs' path
  (let ((brew-bin-path "/opt/homebrew/bin"))
    (when (file-directory-p brew-bin-path)
      (add-to-list 'exec-path brew-bin-path)
      (setenv "PATH" (concat brew-bin-path ":" (getenv "PATH")))))

  ;; SSH agent
  (setenv "SSH_AUTH_SOCK"
          (concat
           (getenv "HOME")
           "/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"))

  ;; Configure macOS modifier keys
  (setq mac-command-modifier 'super  ;; Treat Command key as Super
        mac-option-modifier  'meta)) ;; Treat Option key as Meta

(set-register ?e `(file . ,user-emacs-directory))

;;; Miscellaneous Settings
;; =============================================================================
;; Protect important buffers from accidental closing
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

;; Enable Super (Cmd on macOS) + arrow keys for window navigation
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; Cursor and Line Spacing
(setq-default cursor-type 'hbar)
(setq-default line-spacing 2)

;; General behavior adjustments
(setq Man-notify-method 'bully                ;; Prevent `man` command from switching buffers
      compilation-scroll-output t              ;; Scroll compilation buffer as output arrives
      confirm-kill-emacs 'y-or-n-p             ;; Ask for y/n confirmation when quitting Emacs
      dired-listing-switches "-alFh"           ;; ls switches for Dired (added -h for human-readable)
      display-line-numbers-type 'relative      ;; Use relative numbers
      display-time-default-load-average nil    ;; Don't show load average in time display
      display-time-format " %I:%M%p "          ;; Time format in mode line
      echo-keystrokes 0.02                     ;; Show keystrokes quickly for feedback
      epa-pinentry-mode 'loopback              ;; For GPG agent passphrase entry
      history-delete-duplicates t              ;; Don't store duplicate entries in minibuffer history
      kill-do-not-save-duplicates t            ;; Don't save duplicate entries in kill-ring
      line-move-visual t                       ;; Visual line movement (vs. logical)
      mode-line-percent-position '(-3 "%o")    ;; Position/offset percentage in mode line
      reb-re-syntax 'string                    ;; Use string syntax for `re-builder`
      require-final-newline t                  ;; Ensure files end with a newline
      save-interprogram-paste-before-kill t    ;; Save clipboard contents before killing Emacs
      set-mark-command-repeat-pop t            ;; Pop mark when C-u C-SPC is repeated
      shell-file-name "zsh"                    ;; Default shell for M-x shell
      split-width-threshold nil                ;; Allow splitting windows even if narrow
      tab-stop-list (number-sequence 4 200 4)  ;; Set tab stops at every 4 columns
      vc-follow-symlinks t                     ;; Version control should follow symlinks
      which-key-side-window-location 'right)   ;; Show which-key popup on the right

;; Enhanced Scrolling
(when (and (display-graphic-p) (fboundp 'pixel-scroll-precision-mode))
  (pixel-scroll-precision-mode t)) ;; Smoother scrolling in GUI

(setq scroll-conservatively 100000
      scroll-step 1
      scroll-preserve-screen-position nil)

;; Default buffer-local settings
(setq-default fringe-indicator-alist nil   ;; Remove default fringe indicators (arrows, etc.)
              indent-tabs-mode nil         ;; Use spaces, not tabs, for indentation
              indicate-empty-lines t       ;; Visually indicate empty lines at end of buffer
              tab-width 4                  ;; Set tab width to 4 spaces
              truncate-lines t)            ;; Don't wrap long lines (truncate them)

;; Use y/n instead of yes/no prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable useful global minor modes
(column-number-mode t)             ;; Display column number in the mode line
(delete-selection-mode t)          ;; Typing deletes selected region
(electric-indent-mode t)           ;; Automatic indentation as you type
(electric-pair-mode t)             ;; Automatic closing of pairs (parens, quotes)
(electric-quote-mode t)            ;; Use curly quotes for "" and '' (can be mode-specific)
(global-font-lock-mode t)          ;; Enable syntax highlighting globally
(global-prettify-symbols-mode t)   ;; Replace symbols like 'lambda' with 'λ'
(save-place-mode t)                ;; Reopen files at the last cursor position
(show-paren-mode t)                ;; Highlight matching parentheses
(size-indication-mode t)           ;; Display buffer size in the mode line
(which-function-mode t)            ;; Display current function name in the mode line
(which-key-mode t)                 ;; Show available keybindings after a prefix

;; Calendar Customization
(setq calendar-date-style 'european        ;; Day/Month/Year format
      calendar-week-start-day 1          ;; Start week on Monday
      calendar-today-marker 'calendar-today) ;; How today is marked
;; Show ISO week numbers in the calendar
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'isearch)
      calendar-intermonth-header (propertize "wk" 'font-lock-face 'isearch))
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)

;; Re-enable potentially "dangerous" commands some configurations disable
(dolist (command '(downcase-region upcase-region narrow-to-region widen erase-buffer))
  (put command 'disabled nil))

;; Hooks
;; -----------------------------------------------------------------------------
;; Display line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Display *Help* buffers in the same window, reusing it if possible
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-same-window)
               (reusable-frames . t)))

;; Cleanup whitespace before saving
(setq whitespace-style '(trailing missing-newline-at-eof))
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Recent files
(recentf-mode t)

;; Save recentf list when a client frame is closed
(add-hook 'delete-frame-functions
          (lambda (frame)
            (when (and (daemonp) (frame-parameter frame 'client))
              (recentf-save-list))))

;;; Loading Other Configuration Files
;; =============================================================================
;; Store customisations set via M-x customize in a separate file
(setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))
;; Create custom-file if it doesn't exist, to prevent errors on first run
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; Load modular configuration files
;; Order: custom variables -> package setup -> org-mode -> keybindings
(dolist (file '("init-custom"
                "init-pkgs"
                "init-org"
                "init-keys"))
  (load (locate-user-emacs-file file) t))

;;; end of init.el
