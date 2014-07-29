;;; evil-operator-comment.el --- Comment/uncomment operator for Evil

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: http://github.com/tarao/evil-plugins
;; Version: 0.1
;; Keywords: evil, plugin

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:



(require 'evil)

(defgroup evil-operator-comment nil
  "Comment/uncomment operator for Evil"
  :prefix "evil-operator-comment-"
  :group 'evil)

(defcustom evil-operator-comment-key (kbd "gc")
  "A key for comment/uncomment operator"
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-operator-comment)

(defun evil-mark-on-lines (beg end lines)
  (let ((beg-marker (save-excursion (goto-char beg) (point-marker)))
        (end-marker (save-excursion (goto-char end) (point-marker))))
    (set-marker-insertion-type end-marker t)
    (setcdr lines (cons (cons beg-marker end-marker) (cdr lines)))))

(defun evil-apply-on-block-markers (func beg end &rest args)
  "Like `evil-apply-on-block' but first mark all lines and then
call functions on the marked ranges."
  (let ((lines (list nil)))
    (evil-apply-on-block #'evil-mark-on-lines beg end nil lines)
    (dolist (range (nreverse (cdr lines)))
      (let ((beg (car range)) (end (cdr range)))
        (apply func beg end args)
        (set-marker beg nil)
        (set-marker end nil)))))

(evil-define-operator evil-comment-or-uncomment-region (beg end type)
  "Comment out text from BEG to END with TYPE."
  (interactive "<R>")
  (if (eq type 'block)
      (evil-apply-on-block-markers #'comment-or-uncomment-region beg end)
    (comment-or-uncomment-region beg end))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p) (eq type 'line))
    (evil-first-non-blank)))

;;;###autoload
(define-minor-mode evil-operator-comment-mode
  "Buffer local minor mode of comment/uncomment operator for Evil."
  :lighter ""
  :keymap (make-sparse-keymap)
  :group 'evil-operator-comment
  (evil-normalize-keymaps))

(defun evil-operator-comment-mode-install () (evil-operator-comment-mode 1))

;;;###autoload
(define-globalized-minor-mode global-evil-operator-comment-mode
  evil-operator-comment-mode evil-operator-comment-mode-install
  "Global minor mode of comment/uncomment operator for Evil.")

(evil-define-key 'normal evil-operator-comment-mode-map
                 evil-operator-comment-key 'evil-comment-or-uncomment-region)
(evil-define-key 'visual evil-operator-comment-mode-map
                 evil-operator-comment-key 'evil-comment-or-uncomment-region)

(provide 'evil-operator-comment)
;;; evil-operator-comment.el ends here
