; List of packages for install on launch
(setq package-list '(
  color-theme-solarized
  evil
  navigate
  surround
  flycheck
  evil-leader
))

; Setup the package system
(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

; Refresh the package system if it hasn't ever been before
(unless package-archive-contents
  (package-refresh-contents))

; Install missing plugins
(dolist (plug package-list)
  (unless (package-installed-p plug)
    (package-install plug)))

(global-evil-leader-mode)
(evil-leader/set-leader ",")

(defun close-windows ()
  (interactive)
  (mapc 'delete-buffer-window
    '(
    "*Flycheck errors*"
    "*scratch*"
    )))

(defun delete-buffer-window (name)
  (defvar thing (get-buffer-window name))
  (if thing
    (delete-window thing)))

(evil-leader/set-key "q" 'close-windows)
(evil-leader/set-key "w" 'delete-trailing-whitespace)

; Load evil with C-u
(define-key evil-normal-state-map (kbd "TAB") "%")
(define-key evil-visual-state-map (kbd "TAB") "%")
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(evil-set-toggle-key "C-`")

(require 'flycheck)
(setq flycheck-highlight-mode 'lines)
(add-hook 'after-init-hook #'global-flycheck-mode)

(defun komitee/flycheck-hook ()
  (if (eq (length flycheck-current-errors) 0)
    (if (get-buffer flycheck-error-list-buffer)
        (delete-windows-on flycheck-error-list-buffer))
      (flycheck-list-errors)))
(add-hook 'flycheck-after-syntax-check-hook 'komitee/flycheck-hook)

; Force C-u and C-d number of lines
(define-key evil-motion-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-scroll-up 10)))

(define-key evil-motion-state-map (kbd "C-d")
  (lambda ()
    (interactive)
    (evil-scroll-down 10)))

; Remember your position in files
(require 'saveplace)
(setq-default save-place t)

; Like vim-surround
(require 'surround)
(global-surround-mode 1)

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode 1)

(load-theme 'solarized-dark t)

; Show line numbers with the given format
(global-linum-mode t)
(setq linum-format "%d ")

; Cursor line
(global-hl-line-mode 1)

; Load evil-tmux-navigator
(require 'navigate)

; Mouse stuff
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)

; Translate ansi characters in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; Hide the file menu
(menu-bar-mode -1)

; Set some options
(setq
  auto-save-default nil      ; Don't autosave
  inhibit-startup-message t  ; Don't show the startup stuff
  make-backup-files nil      ; Don't make backups
  vc-follow-symlinks t       ; Auto follow symlinks
  require-final-newline t    ; http://robots.thoughtbot.com/no-newline-at-end-of-file
  indent-tabs-mode nil
)
