;;; bui-utils.el --- General utility functions  -*- lexical-binding: t -*-

;; Copyright © 2014-2016 Alex Kost <alezost@gmail.com>

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

;; This file provides auxiliary functions for "bui.el" package.

;;; Code:

(require 'cl-lib)
(require 'dash)

(defvar bui-true-string "Yes")
(defvar bui-false-string "–")
(defvar bui-list-separator ", ")

(defvar bui-time-format "%F %T"
  "String used to format time values.
For possible formats, see `format-time-string'.")


;;; String utils

(defun bui-get-string (value &optional face)
  "Convert VALUE into a string and return it.

VALUE can be an expression of any type.
If VALUE is t/nil, it is replaced with
`bui-true-string'/`bui-false-string'.
If VALUE is list, its elements are concatenated using
`bui-list-separator'.

If FACE is non-nil, propertize returned string with this FACE."
  (let ((str (cond
              ((stringp value) value)
              ((null value) bui-false-string)
              ((eq t value) bui-true-string)
              ((numberp value) (number-to-string value))
              ((listp value) (mapconcat #'bui-get-string
                                        value
                                        bui-list-separator))
              (t (prin1-to-string value)))))
    (if (and value face)
        (propertize str 'font-lock-face face)
      str)))

(defmacro bui-get-non-nil (&optional value &rest body)
  "Return `bui-false-string' if VALUE is nil, evaluate BODY otherwise."
  (declare (indent 1) (debug t))
  `(if (null ,value)
       bui-false-string
     ,@body))

(defmacro bui-insert-non-nil (&optional value &rest body)
  "Insert `bui-false-string' if VALUE is nil, evaluate BODY otherwise."
  (declare (indent 1) (debug t))
  `(if (null ,value)
       (insert bui-false-string)
     ,@body))

(defun bui-get-time-string (seconds)
  "Return formatted time string from SECONDS.
Use `bui-time-format'."
  (format-time-string bui-time-format (seconds-to-time seconds)))

(defun bui-get-one-line (str)
  "Return one-line string from a multi-line STR."
  (replace-regexp-in-string "\n" " " str))

(defun bui-get-filled-string (str column)
  "Return string by filling STR to COLUMN."
  (with-temp-buffer
    (insert str)
    (let ((fill-column column))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun bui-split-string (str &optional column)
  "Split string STR by lines and return a list of the resulting strings.
If COLUMN is non-nil, fill STR to this column."
  (let ((str (if column
                 (bui-get-filled-string str column)
               str)))
    (split-string str "\n *" t)))

(defun bui-concat-strings (strings separator &optional location)
  "Return new string by concatenating STRINGS with SEPARATOR.
If LOCATION is a symbol `head', add another SEPARATOR to the
beginning of the returned string; if `tail' - add SEPARATOR to
the end of the string; if nil, do not add SEPARATOR; otherwise
add both to the end and to the beginning."
  (let ((str (mapconcat #'identity strings separator)))
    (cond ((null location)
           str)
          ((eq location 'head)
           (concat separator str))
          ((eq location 'tail)
           (concat str separator))
          (t
           (concat separator str separator)))))


;;; Inserting text

;; `bui-newline' exists because `newline' does too much.
(defun bui-newline (&optional n)
  "Insert N (1 by default) number of newlines at point."
  (--dotimes (or n 1)
    (insert "\n")))

(defmacro bui-with-indent (indent &rest body)
  "Evaluate BODY and indent inserted text by INDENT number of spaces."
  (declare (indent 1) (debug t))
  (let ((region-beg-var (make-symbol "region-beg"))
        (indent-var     (make-symbol "indent")))
    `(let ((,region-beg-var (point))
           (,indent-var     ,indent))
       ,@body
       (unless (zerop ,indent-var)
         (indent-rigidly ,region-beg-var (point) ,indent-var)))))

(defun bui-format-insert (value &optional face format)
  "Convert VALUE into a string and insert it at point.
If FACE is non-nil, propertize VALUE with FACE.
If FORMAT is non-nil, format VALUE with FORMAT."
  (let ((str (bui-get-string value face)))
    (insert (if format
                (format format str)
              str))))

(cl-defun bui-mapinsert (function sequence separator &key indent column)
  "Like `mapconcat' but for inserting text.
Apply FUNCTION to each element of SEQUENCE, and insert SEPARATOR
at point between each FUNCTION call.

If INDENT is non-nil, it should be a number of spaces used to
indent each line of the inserted text.

If COLUMN is non-nil, it should be a column number which
shouldn't be exceeded by the inserted text."
  (pcase sequence
    (`(,first . ,rest)
     (let* ((indent (or indent 0))
            (max-column (and column (- column indent))))
       (bui-with-indent indent
         (funcall function first)
         (dolist (element rest)
           (let ((before-sep-pos (and column (point))))
             (insert separator)
             (let ((after-sep-pos (and column (point))))
               (funcall function element)
               (when (and column
                          (> (current-column) max-column))
                 (save-excursion
                   (delete-region before-sep-pos after-sep-pos)
                   (goto-char before-sep-pos)
                   (bui-newline)))))))))))

(defun bui-split-insert (value &optional face column separator)
  "Convert VALUE into a string, split it and insert at point.

If FACE is non-nil, propertize returned string with this FACE.

If COLUMN is non-nil and result string is a one-line string
longer than COLUMN, split it into several short lines.

Separate inserted lines with SEPARATOR."
  (bui-insert-non-nil value
    (let ((strings (bui-split-string (bui-get-string value) column)))
      (bui-mapinsert (lambda (str)
                       (bui-format-insert str face))
                     strings
                     (or separator "")))))


;;; Buttons

(defun bui-button-type? (symbol)
  "Return non-nil, if SYMBOL is a button type."
  (and symbol
       (get symbol 'button-category-symbol)))

(defun bui-insert-button (label &optional type &rest properties)
  "Make button of TYPE with LABEL and insert it at point.
See `insert-text-button' for the meaning of PROPERTIES."
  (apply #'insert-text-button label
         :type (or type 'button)
         properties))

(defun bui-buttonize (value button-type separator &rest properties)
  "Make BUTTON-TYPE button(s) from VALUE.
Return a string with button(s).

VALUE can be nil, a string or a list of strings.  If it is a list
of strings, buttons are separated with SEPARATOR string.

PROPERTIES are passed to `bui-insert-button'."
  (bui-get-non-nil value
    (with-temp-buffer
      (let ((labels (if (listp value) value (list value))))
        (bui-mapinsert (lambda (label)
                         (apply #'bui-insert-button
                                label button-type properties))
                       labels
                       separator))
      (buffer-substring (point-min) (point-max)))))


;;; Files and URLs

(defcustom bui-find-file-function #'find-file
  "Function used to find a file.
The function is called by `bui-find-file' with a file name as a
single argument."
  :type '(choice (function-item find-file)
                 (function-item org-open-file)
                 (function :tag "Other function"))
  :group 'bui)

(defun bui-find-file (file)
  "Find FILE (using `bui-find-file-function') if it exists."
  (if (file-exists-p file)
      (funcall bui-find-file-function file)
    (message "File '%s' does not exist." file)))

(defvar url-handler-regexp)

(defun bui-find-file-or-url (file-or-url)
  "Find FILE-OR-URL."
  (require 'url-handlers)
  (let ((file-name-handler-alist
         (cons (cons url-handler-regexp 'url-file-handler)
               file-name-handler-alist)))
    (find-file file-or-url)))


;;; Symbols, keywords, plists

(defun bui-keyword->symbol (keyword)
  "Transform KEYWORD into symbol (without leading ':')."
  (intern (substring (symbol-name keyword) 1)))

(defun bui-symbol-if-bound (symbol)
  "Return SYMBOL if its value is not void, otherwise return nil."
  (and (boundp symbol) symbol))

(defun bui-make-symbol (&rest symbols)
  "Return symbol by appending SYMBOLS separating them with '-'."
  (intern (mapconcat #'symbol-name symbols "-")))

(defun bui-symbol-title (symbol)
  "Return SYMBOL's name, a string.
This is like `symbol-name', but fancier."
  (if (eq symbol 'id)
      "ID"
    (let ((str (replace-regexp-in-string "-" " " (symbol-name symbol))))
      (concat (capitalize (substring str 0 1))
              (substring str 1)))))

(defmacro bui-plist-let (args varlist &rest body)
  "Parse ARGS, bind variables from VARLIST and eval BODY.

Find keyword values in ARGS, bind them to variables according to
VARLIST, then evaluate BODY.

ARGS is a keyword/value property list.

Each element of VARLIST has a form:

  (SYMBOL KEYWORD [DEFAULT-VALUE])

SYMBOL is a varible name.  KEYWORD is a symbol that will be
searched in ARGS for an according value.  If the value of KEYWORD
does not exist, bind SYMBOL to DEFAULT-VALUE or nil.

The rest arguments (that present in ARGS but not in VARLIST) will
be bound to `%foreign-args' variable.

Example:

  (bui-plist-let '(:two 8 :great ! :bui is)
      ((one :one 1)
       (two :two 2)
       (foo :smth))
    (list one two foo %foreign-args))

  => (1 8 nil (:bui is :great !))"
  (declare (indent 2))
  (let ((args-var (make-symbol "args")))
    `(let (,@(mapcar (lambda (spec)
                       (pcase-let ((`(,name ,_ ,val) spec))
                         (list name val)))
                     varlist)
           (,args-var ,args)
           %foreign-args)
       (while ,args-var
         (pcase ,args-var
           (`(,key ,val . ,rest-args)
            (cl-case key
              ,@(mapcar (lambda (spec)
                          (pcase-let ((`(,name ,key ,_) spec))
                            `(,key (setq ,name val))))
                        varlist)
              (t (setq %foreign-args
                       (cl-list* key val %foreign-args))))
            (setq ,args-var rest-args))))
       ,@body)))

(defun bui-map-plist (function plist)
  "Apply FUNCTION to each keyword/value pair from PLIST.
Return a list of the results."
  ;; (cl-loop for lst on plist by #'cddr
  ;;          collect
  ;;          (let ((key (car lst))
  ;;                (val (cadr lst)))
  ;;            (funcall function key val)))

  ;; Use recursion (and `pcase') instead of the above variant as it is
  ;; more clean and it should be OK for plists as they are not too big.
  (pcase plist
    (`(,key ,val . ,rest)
     (cons (funcall function key val)
           (bui-map-plist function rest)))))


;;; Alist procedures

(defmacro bui-define-alist-accessor (name assoc-fun)
  "Define NAME function to access alist values using ASSOC-FUN."
  `(defun ,name (alist &rest keys)
     ,(format "Return value from ALIST by KEYS using `%s'.
ALIST is alist of alists of alists ... which can be consecutively
accessed with KEYS."
              assoc-fun)
     (if (or (null alist) (null keys))
         alist
       (apply #',name
              (cdr (,assoc-fun (car keys) alist))
              (cdr keys)))))

(bui-define-alist-accessor bui-assq-value assq)
(bui-define-alist-accessor bui-assoc-value assoc)


;;; Misc

(defun bui-copy-as-kill (string &optional no-message?)
  "Put STRING into `kill-ring'.
If NO-MESSAGE? is non-nil, do not display a message about it."
  (kill-new string)
  (unless no-message?
    (message "'%s' has been added to kill ring." string)))

(defmacro bui-define-groups (name &rest args)
  "Define `NAME' and `NAME-faces' customization groups.
NAME should be a symbol.

Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...

Optional keywords:

  - `:parent-group' - name of a parent custom group.

  - `:parent-faces-group' - name of a parent custom faces group.

  - `:group-doc' - docstring of the `NAME' group.

  - `:faces-group-doc' - docstring of the `NAME-faces' group."
  (declare (indent 1))
  (let* ((name-str           (symbol-name name))
         (faces-name         (intern (concat name-str "-faces"))))
    (bui-plist-let args
        ((parent-group       :parent-group 'bui)
         (parent-faces-group :parent-faces-group 'bui-faces)
         (group-doc          :group-doc
                             (format "Settings for '%s' buffers."
                                     name-str))
         (faces-group-doc    :faces-group-doc
                             (format "Faces for '%s' buffers."
                                     name-str)))
      `(progn
         (defgroup ,name nil
           ,group-doc
           :group ',parent-group)
         (defgroup ,faces-name nil
           ,faces-group-doc
           :group ',name
           :group ',parent-faces-group)))))


(defvar bui-utils-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group (or "bui-with-indent"
                           "bui-get-non-nil"
                           "bui-insert-non-nil"
                           "bui-plist-let"
                           "bui-define-groups"))
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode bui-utils-font-lock-keywords)

(provide 'bui-utils)

;;; bui-utils.el ends here
