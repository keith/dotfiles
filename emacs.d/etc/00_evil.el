; Evil leader
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

; Force C-u and C-d number of lines
(define-key evil-motion-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-scroll-up 10)))

(define-key evil-motion-state-map (kbd "C-d")
  (lambda ()
    (interactive)
    (evil-scroll-down 10)))
