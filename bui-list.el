;;; bui-list.el --- 'List' buffer interface for displaying data  -*- lexical-binding: t -*-

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

;; This file provides 'list' buffer interface for displaying an arbitrary
;; data.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'tabulated-list)
(require 'bui)
(require 'bui-info)
(require 'bui-entry)
(require 'bui-utils)

(bui-define-groups bui-list)

(defface bui-list-file-name
  '((t :inherit bui-info-file-name))
  "Face used for file names."
  :group 'bui-list-faces)

(defface bui-list-url
  '((t :inherit bui-info-url))
  "Face used for URLs."
  :group 'bui-list-faces)

(defface bui-list-time
  '((t :inherit bui-info-time))
  "Face used for time stamps."
  :group 'bui-list-faces)

(defun bui-list-describe (&optional mark-names)
  "Describe entries marked with a general mark.
'Describe' means display entries in 'info' buffer.
If no entries are marked, describe the current entry.
With prefix argument, describe entries marked with any mark."
  (interactive (list (unless current-prefix-arg '(general))))
  (let* ((ids        (or (apply #'bui-list-get-marked-id-list mark-names)
                         (list (bui-list-current-id))))
         (count      (length ids))
         (entry-type (bui-current-entry-type)))
    (when (or (<= count (bui-list-describe-warning-count entry-type))
              (y-or-n-p (format "Do you really want to describe %d entries? "
                                count)))
      (bui-list-describe-entries entry-type ids))))


;;; Wrappers for 'list' variables

(defun bui-list-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE and 'list' buffer type."
  (bui-symbol-value entry-type 'list symbol))

(defun bui-list-param-title (entry-type param)
  "Return column title of an ENTRY-TYPE parameter PARAM."
  (bui-param-title entry-type 'list param))

(defun bui-list-format (entry-type)
  "Return column format for ENTRY-TYPE."
  (bui-list-value entry-type 'format))

(defun bui-list-displayed-params (entry-type)
  "Return a list of ENTRY-TYPE parameters that should be displayed."
  (mapcar #'car (bui-list-format entry-type)))

(defun bui-list-sort-key (entry-type)
  "Return sort key for ENTRY-TYPE."
  (bui-list-value entry-type 'sort-key))

(defun bui-list-additional-marks (entry-type)
  "Return alist of additional marks for ENTRY-TYPE."
  (bui-list-value entry-type 'marks))

(defun bui-list-show-single-entry? (entry-type)
  "Return non-nil, if a single entry of ENTRY-TYPE should be listed."
  (bui-list-value entry-type 'show-single))

(defun bui-list-describe-warning-count (entry-type)
  "Return the maximum number of ENTRY-TYPE entries to describe."
  (bui-list-value entry-type 'describe-warning-count))

(defun bui-list-describe-entries (entry-type ids)
  "Describe ENTRY-TYPE entries with IDS in 'info' buffer"
  (funcall (bui-list-value entry-type 'describe-function)
           ids))


;;; Tabulated list internals

(defun bui-list-sort-numerically (column a b)
  "Compare COLUMN of tabulated entries A and B numerically.
This function is used for sort predicates for `tabulated-list-format'.
Return non-nil, if B is bigger than A."
  (cl-flet ((num (entry)
              (string-to-number (aref (cadr entry) column))))
    (> (num b) (num a))))

(defmacro bui-list-define-numerical-sorter (column)
  "Define numerical sort predicate for COLUMN.
See `bui-list-sort-numerically' for details."
  (let ((name (intern (format "bui-list-sort-numerically-%d" column)))
        (doc  (format "\
Predicate to sort tabulated list by column %d numerically.
See `bui-list-sort-numerically' for details."
                      column)))
    `(defun ,name (a b)
       ,doc
       (bui-list-sort-numerically ,column a b))))

(defmacro bui-list-define-numerical-sorters (n)
  "Define numerical sort predicates for columns from 0 to N.
See `bui-list-define-numerical-sorter' for details."
  `(progn
     ,@(mapcar (lambda (i)
                 `(bui-list-define-numerical-sorter ,i))
               (number-sequence 0 n))))

(bui-list-define-numerical-sorters 9)

(defun bui-list-tabulated-sort-key (entry-type)
  "Return ENTRY-TYPE sort key for `tabulated-list-sort-key'."
  (let ((sort-key (bui-list-sort-key entry-type)))
    (and sort-key
         (cons (bui-list-param-title entry-type (car sort-key))
               (cdr sort-key)))))

(defun bui-list-tabulated-vector (entry-type fun)
  "Call FUN on each column specification for ENTRY-TYPE.

FUN is applied to column specification as arguments (see
`bui-list-format').

Return a vector made of values of FUN calls."
  (apply #'vector
         (mapcar (lambda (col-spec)
                   (apply fun col-spec))
                 (bui-list-format entry-type))))

(defun bui-list-tabulated-format (entry-type)
  "Return ENTRY-TYPE list specification for `tabulated-list-format'."
  (bui-list-tabulated-vector
   entry-type
   (lambda (param _ &rest rest-spec)
     (cons (bui-list-param-title entry-type param)
           rest-spec))))

(defun bui-list-tabulated-entries (entries entry-type)
  "Return a list of ENTRY-TYPE values for `tabulated-list-entries'."
  (mapcar (lambda (entry)
            (list (bui-entry-id entry)
                  (bui-list-tabulated-entry entry entry-type)))
          entries))

(defun bui-list-tabulated-entry (entry entry-type)
  "Return array of values for `tabulated-list-entries'.
Parameters are taken from ENTRY-TYPE ENTRY."
  (bui-list-tabulated-vector
   entry-type
   (lambda (param fun &rest _)
     (let ((val (bui-entry-value entry param)))
       (if fun
           (funcall fun val entry)
         (bui-get-string val))))))


;;; Displaying entries

(defun bui-list-get-display-entries (entry-type &rest args)
  "Search for entries and show them in a 'list' buffer preferably."
  (let ((entries (bui-get-entries entry-type 'list args)))
    (if (or (null entries)      ; = 0
            (cdr entries)       ; > 1
            (bui-list-show-single-entry? entry-type)
            (null (bui-symbol-value entry-type 'info 'show-entries-function)))
        (bui-display-entries entries entry-type 'list args 'add)
      (if (equal (bui-symbol-value entry-type 'info 'get-entries-function)
                 (bui-symbol-value entry-type 'list 'get-entries-function))
          (bui-display-entries entries entry-type 'info args 'add)
        (bui-get-display-entries entry-type 'info args 'add)))))

(defun bui-list-insert-entries (entries entry-type)
  "Print ENTRY-TYPE ENTRIES in the current buffer."
  (setq tabulated-list-entries
        (bui-list-tabulated-entries entries entry-type))
  (tabulated-list-print))

(defun bui-list-get-one-line (value &optional _)
  "Return one-line string from a multi-line string VALUE.
VALUE may be nil."
  (bui-get-non-nil value
    (bui-get-one-line value)))

(defun bui-list-get-time (seconds &optional _)
  "Return formatted time string from SECONDS.
SECONDS may be nil."
  (bui-get-non-nil seconds
    (bui-get-string (bui-get-time-string seconds)
                    'bui-list-time)))

(defun bui-list-get-file-name (file-name &optional _)
  "Return FILE-NAME button specification for `tabulated-list-entries'.
FILE-NAME may be nil."
  (bui-get-non-nil file-name
    (list file-name
          'face 'bui-list-file-name
          'action (lambda (btn) (find-file (button-label btn)))
          'follow-link t
          'help-echo "Find file")))

(defun bui-list-get-url (url &optional _)
  "Return URL button specification for `tabulated-list-entries'.
URL may be nil."
  (bui-get-non-nil url
    (list url
          'face 'bui-list-url
          'action (lambda (btn) (browse-url (button-label btn)))
          'follow-link t
          'help-echo "Browse URL")))


;;; 'List' lines

(defun bui-list-current-id ()
  "Return ID of the entry at point."
  (or (tabulated-list-get-id)
      (user-error "No entry here")))

(defun bui-list-current-entry ()
  "Return entry at point."
  (bui-entry-by-id (bui-list-current-id)
                   (bui-current-entries)))

(defun bui-list-for-each-line (fun &rest args)
  "Call FUN with ARGS for each entry line."
  (or (derived-mode-p 'bui-list-mode)
      (error "The current buffer is not in `bui-list-mode'"))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (apply fun args)
      (forward-line))))

(defun bui-list-fold-lines (fun init)
  "Fold over entry lines in the current list buffer.
Call FUN with RESULT as argument for each line, using INIT as
the initial value of RESULT.  Return the final result."
  (let ((res init))
    (bui-list-for-each-line
     (lambda () (setq res (funcall fun res))))
    res))


;;; Marking and sorting

(defvar-local bui-list-marked nil
  "List of the marked entries.
Each element of the list has a form:

  (ID MARK-NAME . ARGS)

ID is an entry ID.
MARK-NAME is a symbol from `bui-list-marks'.
ARGS is a list of additional values.")

(defvar-local bui-list-marks nil
  "Alist of available mark names and mark characters.")

(defvar bui-list-default-marks
  '((empty   . ?\s)
    (general . ?*))
  "Alist of default mark names and mark characters.")

(defun bui-list-marks (entry-type)
  "Return alist of available marks for ENTRY-TYPE."
  (append bui-list-default-marks
          (bui-list-additional-marks entry-type)))

(defun bui-list-get-mark (name)
  "Return mark character by its NAME."
  (or (bui-assq-value bui-list-marks name)
      (error "Mark '%S' not found" name)))

(defun bui-list-get-mark-string (name)
  "Return mark string by its NAME."
  (string (bui-list-get-mark name)))

(defun bui-list-current-mark ()
  "Return mark character of the current line."
  (char-after (line-beginning-position)))

(defun bui-list-get-marked (&rest mark-names)
  "Return list of specs of entries marked with any mark from MARK-NAMES.
Entry specs are elements from `bui-list-marked' list.
If MARK-NAMES are not specified, use all marks from
`bui-list-marks' except the `empty' one."
  (or mark-names
      (setq mark-names
            (delq 'empty (mapcar #'car bui-list-marks))))
  (--filter (-lambda ((_id name . _))
              (memq name mark-names))
            bui-list-marked))

(defun bui-list-get-marked-args (mark-name)
  "Return list of (ID . ARGS) elements from lines marked with MARK-NAME.
See `bui-list-marked' for the meaning of ARGS."
  (mapcar (-lambda ((id _name . args))
            (cons id args))
          (bui-list-get-marked mark-name)))

(defun bui-list-get-marked-id-list (&rest mark-names)
  "Return list of IDs of entries marked with any mark from MARK-NAMES.
See `bui-list-get-marked' for details."
  (mapcar #'car (apply #'bui-list-get-marked mark-names)))

(defun bui-list--mark (mark-name &optional advance &rest args)
  "Put a mark on the current line.
Also add the current entry to `bui-list-marked' using its ID and ARGS.
MARK-NAME is a symbol from `bui-list-marks'.
If ADVANCE is non-nil, move forward by one line after marking."
  (let ((id (bui-list-current-id)))
    (if (eq mark-name 'empty)
        (setq bui-list-marked (assq-delete-all id bui-list-marked))
      (let ((assoc (assq id bui-list-marked))
            (val (cons mark-name args)))
        (if assoc
            (setcdr assoc val)
          (push (cons id val) bui-list-marked)))))
  (tabulated-list-put-tag (bui-list-get-mark-string mark-name)
                          advance))

(defun bui-list-mark (&optional arg)
  "Mark the current line and move to the next line.
With ARG, mark all lines."
  (interactive "P")
  (if arg
      (bui-list-mark-all)
    (bui-list--mark 'general t)))

(defun bui-list-mark-all (&optional mark-name)
  "Mark all lines with MARK-NAME mark.
MARK-NAME is a symbol from `bui-list-marks'.
Interactively, put a general mark on all lines."
  (interactive)
  (or mark-name (setq mark-name 'general))
  (bui-list-for-each-line #'bui-list--mark mark-name))

(defun bui-list-unmark (&optional arg)
  "Unmark the current line and move to the next line.
With ARG, unmark all lines."
  (interactive "P")
  (if arg
      (bui-list-unmark-all)
    (bui-list--mark 'empty t)))

(defun bui-list-unmark-backward ()
  "Move up one line and unmark it."
  (interactive)
  (forward-line -1)
  (bui-list--mark 'empty))

(defun bui-list-unmark-all ()
  "Unmark all lines."
  (interactive)
  (bui-list-mark-all 'empty))

(defun bui-list-restore-marks ()
  "Put marks according to `bui-list-marked'."
  (bui-list-for-each-line
   (lambda ()
     (let ((mark-name (car (bui-assq-value bui-list-marked
                                           (bui-list-current-id)))))
       (tabulated-list-put-tag
        (bui-list-get-mark-string (or mark-name 'empty)))))))

(defun bui-list-sort (&optional n)
  "Sort list entries by the column at point.
With a numeric prefix argument N, sort the Nth column.
Same as `tabulated-list-sort', but also restore marks after sorting."
  (interactive "P")
  (tabulated-list-sort n)
  (bui-list-restore-marks))


;;; Major mode and interface definer

(defvar bui-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap bui-map
                               tabulated-list-mode-map))
    (define-key map (kbd "RET") 'bui-list-describe)
    (define-key map (kbd "i")   'bui-list-describe)
    (define-key map (kbd "m")   'bui-list-mark)
    (define-key map (kbd "*")   'bui-list-mark)
    (define-key map (kbd "M")   'bui-list-mark-all)
    (define-key map (kbd "u")   'bui-list-unmark)
    (define-key map (kbd "DEL") 'bui-list-unmark-backward)
    (define-key map (kbd "U")   'bui-list-unmark-all)
    (define-key map [remap tabulated-list-sort] 'bui-list-sort)
    map)
  "Keymap for `bui-list-mode' buffers.")

(define-derived-mode bui-list-mode tabulated-list-mode "BUI-List"
  "Parent mode for displaying data in 'list' form.")

(defun bui-list-mode-initialize (entry-type)
  "Set up the current 'list' buffer for displaying ENTRY-TYPE entries."
  (setq tabulated-list-padding  2
        tabulated-list-format   (bui-list-tabulated-format entry-type)
        tabulated-list-sort-key (bui-list-tabulated-sort-key entry-type))
  (setq-local bui-list-marks (bui-list-marks entry-type))
  (tabulated-list-init-header))

(defmacro bui-define-list-interface (entry-type &rest args)
  "Define 'list' interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...

Required keywords:

  - `:format' - default value of the generated
    `ENTRY-TYPE-list-format' variable.

Optional keywords:

  - `:sort-key' - default value of the generated
    `ENTRY-TYPE-list-sort-key' variable.

  - `:describe-function' - default value of the generated
    `ENTRY-TYPE-describe-function' variable.

  - `:describe-count' - default value of the generated
    `ENTRY-TYPE-describe-warning-count' variable.

  - `:list-single?' - default value of the generated
    `ENTRY-TYPE-list-show-single' variable.

  - `:marks' - default value of the generated
    `ENTRY-TYPE-list-marks' variable.

The rest keyword arguments are passed to
`bui-define-interface' macro."
  (declare (indent 1))
  (let* ((entry-type-str     (symbol-name entry-type))
         (prefix             (concat entry-type-str "-list"))
         (group              (intern prefix))
         (describe-var       (intern (concat prefix "-describe-function")))
         (describe-count-var (intern (concat prefix
                                             "-describe-warning-count")))
         (format-var         (intern (concat prefix "-format")))
         (sort-key-var       (intern (concat prefix "-sort-key")))
         (list-single-var    (intern (concat prefix "-show-single")))
         (marks-var          (intern (concat prefix "-marks"))))
    (bui-plist-let args
        ((describe-val       :describe-function)
         (describe-count-val :describe-count 10)
         (format-val         :format)
         (sort-key-val       :sort-key)
         (list-single-val    :list-single?)
         (marks-val          :marks))
      `(progn
         (defcustom ,format-var ,format-val
           ,(format "\
List of format values of the displayed columns.
Each element of the list has a form:

  (PARAM VALUE-FUN WIDTH SORT . PROPS)

PARAM is a name of '%s' entry parameter.

VALUE-FUN may be either nil or a function returning a value that
will be inserted.  The function is called with 2 arguments: the
first one is the value of the parameter; the second one is an
entry (alist of parameter names and values).

For the meaning of WIDTH, SORT and PROPS, see
`tabulated-list-format'."
                    entry-type-str)
           :type 'sexp
           :group ',group)

         (defcustom ,sort-key-var ,sort-key-val
           ,(format "\
Default sort key for 'list' buffer with '%s' entries.
Should be nil (no sort) or have a form:

  (PARAM . FLIP)

PARAM is the name of '%s' entry parameter.  For the meaning of
FLIP, see `tabulated-list-sort-key'."
                    entry-type-str entry-type-str)
           :type '(choice (const :tag "No sort" nil)
                          (cons symbol boolean))
           :group ',group)

         (defvar ,marks-var ,marks-val
           ,(format "\
Alist of additional marks for 'list' buffer with '%s' entries.
Marks from this list are used along with `bui-list-default-marks'."
                    entry-type-str))

         (defcustom ,list-single-var ,list-single-val
           ,(format "\
If non-nil, list '%s' entry even if it is the only matching result.
If nil, show a single '%s' entry in the 'info' buffer."
                    entry-type-str entry-type-str)
           :type 'boolean
           :group ',group)

         (defcustom ,describe-count-var ,describe-count-val
           ,(format "\
The maximum number of '%s' entries to describe without a warning.
If you want to describe more than this number of marked entries,
you will be prompted for confirmation.  See also
`bui-list-describe'."
                    entry-type-str)
           :type 'integer
           :group ',group)

         (defvar ,describe-var ,describe-val
           ,(format "Function used to describe '%s' entries."
                    entry-type-str))

         (bui-define-interface ,entry-type list
           ,@%foreign-args)))))


(defvar bui-list-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group "bui-define-list-interface")
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode bui-list-font-lock-keywords)

(provide 'bui-list)

;;; bui-list.el ends here
