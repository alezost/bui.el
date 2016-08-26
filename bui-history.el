;;; bui-history.el --- Buffer history  -*- lexical-binding: t -*-

;; Copyright © 2014 Alex Kost <alezost@gmail.com>

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

;; This file provides a general support for buffer history similar to
;; the history of a `help-mode' buffer.

;;; Code:

(require 'cl-lib)

(defvar-local bui-history-stack-item nil
  "Current item of the history.
A list of the form (FUNCTION [ARGS ...]).
The item is used by calling (apply FUNCTION ARGS).")
(put 'bui-history-stack-item 'permanent-local t)

(defvar-local bui-history-back-stack nil
  "Stack (list) of visited items.
Each element of the list has a form of `bui-history-stack-item'.")
(put 'bui-history-back-stack 'permanent-local t)

(defvar-local bui-history-forward-stack nil
  "Stack (list) of items visited with `bui-history-back'.
Each element of the list has a form of `bui-history-stack-item'.")
(put 'bui-history-forward-stack 'permanent-local t)

(defvar bui-history-size 0
  "Maximum number of items saved in history.
If 0, the history is disabled.")

(defun bui-history-add (item)
  "Add ITEM to history."
  (and bui-history-stack-item
       (push bui-history-stack-item bui-history-back-stack))
  (setq bui-history-forward-stack nil
        bui-history-stack-item item)
  (when (>= (length bui-history-back-stack)
            bui-history-size)
    (setq bui-history-back-stack
          (cl-loop for elt in bui-history-back-stack
                   for i from 1 to bui-history-size
                   collect elt))))

(defun bui-history-replace (item)
  "Replace current item in history with ITEM."
  (setq bui-history-stack-item item))

(defun bui-history-goto (item)
  "Go to the ITEM of history.
ITEM should have the form of `bui-history-stack-item'."
  (or (listp item)
      (error "Wrong value of history element"))
  (setq bui-history-stack-item item)
  (apply (car item) (cdr item)))

(defun bui-history-back ()
  "Go back to the previous element of history in the current buffer."
  (interactive)
  (or bui-history-back-stack
      (user-error "No previous element in history"))
  (push bui-history-stack-item bui-history-forward-stack)
  (bui-history-goto (pop bui-history-back-stack)))

(defun bui-history-forward ()
  "Go forward to the next element of history in the current buffer."
  (interactive)
  (or bui-history-forward-stack
      (user-error "No next element in history"))
  (push bui-history-stack-item bui-history-back-stack)
  (bui-history-goto (pop bui-history-forward-stack)))

(provide 'bui-history)

;;; bui-history.el ends here
