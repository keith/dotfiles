; List of packages for install on launch
(setq package-list '(
  ag
  color-theme-solarized
  evil
  evil-leader
  flycheck
  helm
  helm-projectile
  init-loader
  markdown-mode
  navigate
  projectile
  surround
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

; Cursor line
(global-hl-line-mode 1)

; Translate ansi characters in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; Hide the file menu
(menu-bar-mode -1)

; Set some options
(setq make-backup-files nil)      ; Don't make backups
(setq
  auto-save-default nil      ; Don't autosave
  inhibit-startup-message t  ; Don't show the startup stuff
  vc-follow-symlinks t       ; Auto follow symlinks
  require-final-newline t    ; http://robots.thoughtbot.com/no-newline-at-end-of-file
  indent-tabs-mode nil
)

(require 'init-loader)
(init-loader-load "~/.emacs.d/etc/")
