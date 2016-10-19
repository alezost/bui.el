;;; buffers.el --- List of buffers and buffer info

;; Copyright © 2016 Alex Kost <alezost@gmail.com>

;; This program is free software; you can redistribute it and/or modify
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

;;; Commentary:

;; This is an example of using BUI (Buffer User Interface) library.
;;
;; It shows how to use bui to display a list of buffers à la
;; `list-buffers' or `ibuffer'.
;;
;; To try it, load this file (for example, with "M-x load-file"), and
;; run "M-x buffers" command.  There you can mark several buffers (with
;; "m") and press "i" to display the info buffer; press "f f" to enable
;; fiters, etc.

;;; Code:

(require 'bui)
(require 'bui-info)
(require 'bui-list)
(require 'help-mode)  ; for `help-function' button

(defun buffers-get-buffers (&optional search-type &rest search-values)
  (or search-type (setq search-type 'all))
  (cl-case search-type
    (all (buffer-list))
    (id search-values)
    (t (error "Unknown search type: %S" search-type))))

(defun buffers-buffer-file-name ()
  (or buffer-file-name
      (and (boundp 'dired-directory)
           (if (stringp dired-directory)
               dired-directory
             (car dired-directory)))))

(defun buffers-visited-file-modtime ()
  (let ((time (visited-file-modtime)))
    (cl-case time
      ((-1 0) nil)
      (t time))))

(defun buffers-buffer->entry (buffer)
  (with-current-buffer buffer
    `((id   . ,buffer)
      (name . ,(buffer-name))
      (mode . ,major-mode)
      (size . ,(buffer-size))
      (mod-time  . ,(buffers-visited-file-modtime))
      (file-name . ,(buffers-buffer-file-name)))))

(defun buffers-get-entries (&rest args)
  (mapcar #'buffers-buffer->entry
          (apply #'buffers-get-buffers args)))

(bui-define-entry-type buffers
  :titles '((mod-time . "Modification Time"))
  :get-entries-function #'buffers-get-entries
  :filter-predicates
  '(buffers-buffer-ephemeral?
    buffers-buffer-non-ephemeral?
    buffers-buffer-visiting-file?
    buffers-buffer-not-visiting-file?))

(defun buffers-describe-mode-function (button)
  (describe-function (intern (button-label button))))


;;; Filter predicates

(defun buffers-buffer-ephemeral? (entry)
  "Return non-nil, if ENTRY's buffer name starts with a space."
  (string= " " (substring (bui-entry-value entry 'name) 0 1)))

(defun buffers-buffer-non-ephemeral? (entry)
  "Return non-nil, if ENTRY's buffer name does not start with a space."
  (not (buffers-buffer-ephemeral? entry)))

(defun buffers-buffer-visiting-file? (entry)
  "Return non-nil, if ENTRY's buffer visits a file."
  (bui-entry-non-void-value entry 'file-name))

(defun buffers-buffer-not-visiting-file? (entry)
  "Return non-nil, if ENTRY's buffer does not visit a file."
  (not (buffers-buffer-visiting-file? entry)))


;;; 'Info' interface

(bui-define-interface buffers info
  :format '((name format buffers-info-insert-name)
            (mode format (simple buffers-mode-function))
            (size format (format))
            nil
            (file-name nil (simple bui-file))
            (mod-time format (time))))

(define-button-type 'buffers-mode-function
  :supertype 'help-function
  'action 'buffers-describe-mode-function)

(defun buffers-info-insert-name (name entry)
  (bui-info-insert-value-simple (bui-entry-value entry 'name)
                                'mode-line-buffer-id)
  (bui-insert-indent)
  (bui-insert-action-button
   "Switch"
   (lambda (btn)
     (pop-to-buffer (button-get btn 'buffer)))
   "Switch to this buffer"
   'buffer (bui-entry-id entry)))


;;; 'List' interface

(bui-define-interface buffers list
  :buffer-name "*Buffers*"
  :describe-function #'buffers-list-describe
  :titles '((mod-time . "Mod. Time"))
  :format '((name nil 30 t)
            (mode buffers-list-get-mode 25 t)
            (size nil 8 bui-list-sort-numerically-2 :right-align t)
            ;; (mod-time bui-list-get-time 20 t)
            (file-name bui-list-get-file-name 30 t))
  :sort-key '(name))

(let ((map buffers-list-mode-map))
  (define-key map (kbd "RET") 'buffers-list-switch-to-buffer)
  (define-key map (kbd "k")   'buffers-list-kill-buffers))

(defun buffers-list-get-mode (mode &optional _)
  "Return MODE button specification for `tabulated-list-entries'.
MODE may be nil."
  (list (symbol-name mode)
        :supertype 'help-function
        'action 'buffers-describe-mode-function))

(defun buffers-list-describe (&rest buffers)
  "Display 'info' buffer for BUFFERS."
  (bui-get-display-entries 'buffers 'info (cons 'id buffers)))

(defun buffers-list-switch-to-buffer ()
  (interactive)
  (pop-to-buffer (bui-list-current-id)))

(defun buffers-list-kill-buffers ()
  "Kill marked buffers (or the current buffer)."
  (interactive)
  (dolist (buffer (or (bui-list-get-marked-id-list)
                      (list (bui-list-current-id))))
    (kill-buffer buffer))
  (revert-buffer nil t))


;;; Interactive commands

;;;###autoload
(defun buffers ()
  "Display a list of buffers."
  (interactive)
  (bui-list-get-display-entries 'buffers))

(provide 'buffers)

;;; buffers.el ends here
