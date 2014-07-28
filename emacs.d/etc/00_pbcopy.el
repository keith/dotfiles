;;; pbcopy.el --- Emacs Interface to pbcopy

;; Copyright (C) 2011 Daniel Nelson, based on xclip.el, by Leo Shidai Liu

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Enables the kill-ring to interact with the clipboard when running
;; Emacs from a Mac OSX terminal (without losing full kill-ring
;; functionality). All I did was modify xclip.el to work with pbcopy
;; and pbpaste. The real thanks go to Leo Shidai Liu, the author of
;; xclip.el.

;; Activate via:
;; (require 'pbcopy)
;; (turn-on-pbcopy)


;;; Code:
(defvar pbcopy-program (executable-find "pbcopy")
  "Name of Pbcopy program tool.")
(defvar pbpaste-program (executable-find "pbpaste")
  "Name of Pbpaste program tool.")

(defvar pbcopy-select-enable-clipboard t
  "Non-nil means cutting and pasting uses the clipboard.
This is in addition to, but in preference to, the primary selection.")

(defvar pbcopy-last-selected-text-clipboard nil
  "The value of the CLIPBOARD X selection from pbcopy.")

(defvar pbcopy-last-selected-text-primary nil
  "The value of the PRIMARY X selection from pbcopy.")

(defun pbcopy-set-selection (type data)
  "TYPE is a symbol: primary, secondary and clipboard.

See `x-set-selection'."
  (when pbcopy-program
    (let* ((process-connection-type nil)
           (proc (start-process "pbcopy" nil "pbcopy")))
      (process-send-string proc data)
      (process-send-eof proc))))

(defun pbcopy-select-text (text &optional push)
  "See `x-select-text'."
  (pbcopy-set-selection 'primary text)
  (setq pbcopy-last-selected-text-primary text)
  (when pbcopy-select-enable-clipboard
    (pbcopy-set-selection 'clipboard text)
    (setq pbcopy-last-selected-text-clipboard text)))

(defun pbcopy-selection-value ()
  "See `x-cut-buffer-or-selection-value'."
  (when pbcopy-program
    (let (clip-text primary-text)
      (when pbcopy-select-enable-clipboard
        (setq clip-text (shell-command-to-string "pbpaste"))
        (setq clip-text
              (cond ;; check clipboard selection
               ((or (not clip-text) (string= clip-text ""))
                (setq pbcopy-last-selected-text-primary nil))
               ((eq      clip-text pbcopy-last-selected-text-clipboard) nil)
               ((string= clip-text pbcopy-last-selected-text-clipboard)
                ;; Record the newer string,
                ;; so subsequent calls can use the `eq' test.
                (setq pbcopy-last-selected-text-clipboard clip-text)
                nil)
               (t (setq pbcopy-last-selected-text-clipboard clip-text)))))
      (setq primary-text (shell-command-to-string "pbpaste"))
      (setq primary-text
            (cond ;; check primary selection
             ((or (not primary-text) (string= primary-text ""))
              (setq pbcopy-last-selected-text-primary nil))
             ((eq      primary-text pbcopy-last-selected-text-primary) nil)
             ((string= primary-text pbcopy-last-selected-text-primary)
              ;; Record the newer string,
              ;; so subsequent calls can use the `eq' test.
              (setq pbcopy-last-selected-text-primary primary-text)
              nil)
             (t (setq pbcopy-last-selected-text-primary primary-text))))
      (or clip-text primary-text))))

;;;###autoload
(defun turn-on-pbcopy ()
  (interactive)
  (setq interprogram-cut-function 'pbcopy-select-text)
  (setq interprogram-paste-function 'pbcopy-selection-value))

;;;###autoload
(defun turn-off-pbcopy ()
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))


(add-hook 'terminal-init-xterm-hook 'turn-on-pbcopy)


(provide 'pbcopy)