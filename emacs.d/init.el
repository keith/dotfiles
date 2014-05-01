; List of packages for install on launch
(setq package-list '(
    color-theme-solarized
    evil
    navigate
    surround
  ))

; Setup the package system
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

; Refresh the package system if it hasn't ever been before
(unless package-archive-contents
  (package-refresh-contents))

; Install missing plugins
(dolist (plug package-list)
  (unless (package-installed-p plug)
    (package-install plug)))

; Load evil with C-u
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

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

; Load solarized based on the time of day
(defun set-theme-on-time ()
    (let ((time
	    (string-to-number
	      (format-time-string "%H"))))
      (if
	(and
	  (>= time 7)
	  (< time 19))
	    (load-theme 'solarized-light t)
	(load-theme 'solarized-dark t))))

(defun strip-string-from-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (replace-regexp-in-string "\n" "" (buffer-string))))

(defun set-theme-on-file (file)
  (load-theme
   (read (concat "solarized-"
	   (strip-string-from-file file)))
   t))

(if (file-readable-p "~/.coloroverride")
    (set-theme-on-file "~/.coloroverride")
    (set-theme-on-time))

; Show line numbers with the given format
(global-linum-mode t)
(setq linum-format "%d ")

; Load evil-tmux-navigator
(require 'navigate)

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
  )
