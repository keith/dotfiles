(require 'pbcopy)
(turn-on-pbcopy)

; Load evil-tmux-navigator
(require 'navigate)

; Remember your position in files
(require 'saveplace)
(setq-default save-place t)

; Like vim-surround
(require 'surround)
(global-surround-mode 1)

; (require 'linum)
; (require 'linum+)
; (require 'evil-relative-linum)
; (evil-relative-linum-on)
; (evil-relative-linum-activate)
(require 'linum-relative)

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode 1)

(require 'evil-operator-comment)
(global-evil-operator-comment-mode 1)
