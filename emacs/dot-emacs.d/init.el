;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Environment Setup
;; =============================================================================
(set-language-environment "UTF-8")
(setq default-input-method nil)

(setq default-directory "~/")

;; Centralize temporary files (backups, auto-saves)
(defconst my:emacs-tmp-dir
  (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory)
  "Directory for Emacs temporary files like backups and auto-saves.")
(make-directory my:emacs-tmp-dir t)

(setq backup-directory-alist `((".*" . ,my:emacs-tmp-dir))
      auto-save-file-name-transforms `((".*" ,my:emacs-tmp-dir t))
      auto-save-list-file-prefix my:emacs-tmp-dir
      create-lockfiles nil)

;; macOS Specific Environment
(when (eq system-type 'darwin)
  ;; Add Homebrew executables to Emacs' path
  (let ((brew-bin-path "/opt/homebrew/bin"))
    (when (file-directory-p brew-bin-path)
      (add-to-list 'exec-path brew-bin-path)
      (setenv "PATH" (concat brew-bin-path ":" (getenv "PATH")))))

  ;; SSH agent (1Password)
  (setenv "SSH_AUTH_SOCK"
          (concat (getenv "HOME")
                  "/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"))

  ;; Configure macOS modifier keys
  (setq mac-command-modifier 'super
        mac-option-modifier  'meta))

(set-register ?e `(file . ,user-emacs-directory))

;;; Miscellaneous Settings
;; =============================================================================
;; Protect important buffers from accidental closing
(with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*" (emacs-lock-mode 'kill))

;; Window navigation with Super + arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))

;; Cursor and Line Spacing
(setq-default cursor-type 'hbar)
(setq-default line-spacing 2)

;; General behavior adjustments
(setq Man-notify-method 'bully
      compilation-scroll-output t
      confirm-kill-emacs 'y-or-n-p
      dired-listing-switches "-alFh"
      display-line-numbers-type 'relative
      display-time-default-load-average nil
      display-time-format " %I:%M%p "
      echo-keystrokes 0.02
      epa-pinentry-mode 'loopback
      history-delete-duplicates t
      kill-do-not-save-duplicates t
      line-move-visual t
      mode-line-percent-position '(-3 "%o")
      reb-re-syntax 'string
      require-final-newline t
      save-interprogram-paste-before-kill t
      set-mark-command-repeat-pop t
      shell-file-name "zsh"
      split-width-threshold nil
      tab-stop-list (number-sequence 4 200 4)
      vc-follow-symlinks t
      which-key-side-window-location 'right)

;; Enhanced Scrolling
(when (and (display-graphic-p) (fboundp 'pixel-scroll-precision-mode))
  (pixel-scroll-precision-mode t))

(setq scroll-conservatively 100000
      scroll-step 1
      scroll-preserve-screen-position nil)

;; Default buffer-local settings
(setq-default fringe-indicator-alist nil
              indent-tabs-mode nil
              indicate-empty-lines t
              tab-width 4
              truncate-lines t)

;; Enable useful global minor modes
(column-number-mode t)
(delete-selection-mode t)
(electric-indent-mode t)
(electric-pair-mode t)
(global-font-lock-mode t)
(global-prettify-symbols-mode t)
(save-place-mode t)
(show-paren-mode t)
(size-indication-mode t)
(which-function-mode t)
(which-key-mode t)

;; Built-in isearch match count (replaces anzu)
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil)

;; Calendar Customization
(setq calendar-date-style 'european
      calendar-week-start-day 1
      calendar-today-marker 'calendar-today)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'isearch)
      calendar-intermonth-header (propertize "wk" 'font-lock-face 'isearch))
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)

;; Re-enable potentially "dangerous" commands
(dolist (command '(downcase-region upcase-region narrow-to-region widen erase-buffer))
  (put command 'disabled nil))

;; Hooks
;; -----------------------------------------------------------------------------
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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
(setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; Load modular configuration files
(dolist (file '("init-custom"
                "init-pkgs"
                "init-org"
                "init-keys"))
  (load (locate-user-emacs-file file) t))

;;; end of init.el
