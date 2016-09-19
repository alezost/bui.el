;;; bui.el --- Buffer interface for displaying data  -*- lexical-binding: t -*-

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

;; This file provides a general 'buffer' interface for displaying an
;; arbitrary data.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'bui-history)
(require 'bui-utils)

(bui-define-groups bui
  :parent-group external
  :parent-faces-group faces
  :group-doc "Settings for Buffer User Interface.")

(defvar bui-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") 'bui-history-back)
    (define-key map (kbd "C-c C-f") 'bui-history-forward)
    (define-key map (kbd "l") 'bui-history-back)
    (define-key map (kbd "r") 'bui-history-forward)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "R") 'bui-redisplay)
    (define-key map (kbd "f") 'bui-filter-map)
    map)
  "Parent keymap for all BUI modes.")


;;; Buffer item

(cl-defstruct (bui-item
               (:constructor nil)
               (:constructor bui-make-item
                             (entries entry-type buffer-type args))
               (:copier      nil))
  entries entry-type buffer-type args)

(defvar-local bui-item nil
  "Data (structure) for the current BUI buffer.
The structure consists of the following elements:

- `entries': list of the currently displayed entries.

  Each element of the list is an alist with an entry data of the
  following form:

    ((PARAM . VAL) ...)

  PARAM is a name of the entry parameter.
  VAL is a value of this parameter.

- `entry-type': type of the currently displayed entries.

- `buffer-type': type of the current buffer.

- `args': arguments used to get the current entries.")
(put 'bui-item 'permanent-local t)

(defmacro bui-with-item (item &rest body)
  "Evaluate BODY using buffer ITEM.
The following local variables are available inside BODY:
`%entries', `%buffer-type', `%entry-type', `%args'.
See `bui-item' for details."
  (declare (indent 1) (debug t))
  (let ((item-var (make-symbol "item")))
    `(let ((,item-var ,item))
       (let ((%entries     (bui-item-entries     ,item-var))
             (%entry-type  (bui-item-entry-type  ,item-var))
             (%buffer-type (bui-item-buffer-type ,item-var))
             (%args        (bui-item-args        ,item-var)))
         ,@body))))

(defmacro bui-with-current-item (&rest body)
  "Evaluate BODY using `bui-item'.
See `bui-with-item' for details."
  (declare (indent 0) (debug t))
  `(bui-with-item bui-item
     ,@body))

(defmacro bui-define-current-item-accessor (name)
  "Define `bui-current-NAME' function to access NAME
element of `bui-item' structure.
NAME should be a symbol."
  (let* ((name-str (symbol-name name))
         (accessor (intern (concat "bui-item-" name-str)))
         (fun-name (intern (concat "bui-current-" name-str)))
         (doc      (format "\
Return '%s' of the current BUI buffer.
See `bui-item' for details."
                           name-str)))
    `(defun ,fun-name ()
       ,doc
       (and bui-item
            (,accessor bui-item)))))

(defmacro bui-define-current-item-accessors (&rest names)
  "Define `bui-current-NAME' functions for NAMES.
See `bui-define-current-item-accessor' for details."
  `(progn
     ,@(mapcar (lambda (name)
                 `(bui-define-current-item-accessor ,name))
               names)))

(bui-define-current-item-accessors
 entries entry-type buffer-type args)

(defmacro bui-define-current-args-accessor (n prefix name)
  "Define `PREFIX-NAME' function to access Nth element of 'args'
field of `bui-item' structure.
PREFIX and NAME should be symbols."
  (let* ((prefix-str (symbol-name prefix))
         (name-str   (symbol-name name))
         (fun-name   (intern (concat prefix-str "-" name-str)))
         (doc        (format "\
Return '%s' of the current buffer.
'%s' is the element number %d in 'args' field of `bui-item'."
                             name-str name-str n)))
    `(defun ,fun-name ()
       ,doc
       (nth ,n (bui-current-args)))))

(defmacro bui-define-current-args-accessors (prefix &rest names)
  "Define `PREFIX-NAME' functions for NAMES.
See `bui-define-current-args-accessor' for details."
  `(progn
     ,@(cl-loop for name in names
                for i from 0
                collect `(bui-define-current-args-accessor
                          ,i ,prefix ,name))))


;;; Filtering

(defvar bui-filter-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'bui-enable-filter)
    (define-key map (kbd "d") 'bui-disable-filters)
    map)
  "Keymap with filter commands for BUI modes.")
(fset 'bui-filter-map bui-filter-map)

(defcustom bui-filter-mode-line-string "(f)"
  "String displayed in the mode line when filters are enabled.
Set it to nil, if you don't want to display such a string."
  :type '(choice string (const nil))
  :group 'bui)

(defvar-local bui-active-filter-predicates nil
  "List of the active filter predicates.
These predicates are used to hide unneeded entries from the
current buffer.  Each buffer entry is passed (as a single
argument) through these predicates in turn.  If a predicate
returns nil, the entry will be hidden (the rest predicates are
not called), otherwise the entry \"survives\" this predicate and
it is passed to the next one, and so on.")
(put 'bui-active-filter-predicates 'permanent-local t)

(defun bui-filter-current-entries (&rest predicates)
  "Filter the current entries using PREDICATES, and redisplay them.
If PREDICATES are not specified, display all entries."
  (setq bui-active-filter-predicates predicates)
  (bui-show-entries (bui-current-entries)
                    (bui-current-entry-type)
                    (bui-current-buffer-type)))

(defun bui-enable-filter (predicate &optional single?)
  "Apply filter PREDICATE to the current entries.
Interactively, prompt for PREDICATE, choosing candidates from the
available predicates.

If SINGLE? is non-nil (with prefix argument), make PREDICATE the
only active one (remove the other active predicates)."
  (interactive
   (list (intern (completing-read
                  (if current-prefix-arg
                      "Enable single filter predicate: "
                    "Add filter predicate: ")
                  (bui-available-filter-predicates
                   (bui-current-entry-type)
                   (bui-current-buffer-type))))
         current-prefix-arg))
  (or (functionp predicate)
      (error "Wrong filter predicate: %S" predicate))
  (if (if single?
          (equal (list predicate) bui-active-filter-predicates)
        (memq predicate bui-active-filter-predicates))
      (message "Filter predicate '%S' already enabled" predicate)
    (apply #'bui-filter-current-entries
           (if single?
               (list predicate)
             (cons predicate bui-active-filter-predicates)))))

(defun bui-disable-filters ()
  "Disable all active filters."
  (interactive)
  (if (null bui-active-filter-predicates)
      (message "There are no active filters.")
    (bui-filter-current-entries)))


;;; General variables

(defcustom bui-titles nil
  "Alist of titles of parameters."
  :type '(alist :key-type symbol :value-type string)
  :group 'bui)

(defvar bui-boolean-params nil
  "List of boolean parameters.
These parameters are displayed using `bui-false-string' for
nil values (unlike usual parameters which are displayed using
`bui-empty-string').")

(defvar bui-get-entries-function nil
  "Function used to receive entries.")

(defvar bui-show-entries-function nil
  "Function used to show entries.
This function is called with a list of entries as a single
argument.  If nil, `bui-show-entries-default' is called with
appropriate ENTRY-TYPE and BUFFER-TYPE.")

(defvar bui-mode-initialize-function nil
  "Function used to set up the buffer.
This function is called without arguments after enabling the
mode (right before running mode hooks).  If nil,
`bui-initialize-mode-default' is called with appropriate
ENTRY-TYPE and BUFFER-TYPE.")

(defvar bui-message-function nil
  "Function used to display a message after showing entries.
If nil, do not display messages.")

(defcustom bui-buffer-name nil
  "Default name of a buffer for displaying entries.
May be nil, a string or a function returning a string.  The
function is called with the same arguments as the function used
to get entries.  If nil, the name is defined automatically."
  :type '(choice string function (const nil))
  :group 'bui)

(defcustom bui-filter-predicates nil
  "List of available filter predicates.
These predicates are used as completions for
'\\[bui-enable-filter]' command to hide entries. See
`bui-active-filter-predicates' for details."
  :type '(repeat function)
  :group 'bui)

(defcustom bui-revert-confirm t
  "If non-nil, ask to confirm for reverting the buffer."
  :type 'boolean
  :group 'bui)


;;; Overriding variables

(defconst bui-entry-symbol-specifications
  '((:true-string           true-string t)
    (:false-string          false-string t)
    (:empty-string          empty-string t)
    (:list-separator        list-separator t)
    (:time-format           time-format t)
    (:filter-predicates     filter-predicates t)
    (:boolean-params        boolean-params))
  "Specifications for generating entry variables.
See `bui-symbol-specifications' for details.")

(defconst bui-symbol-specifications
  '((:get-entries-function  get-entries-function)
    (:show-entries-function show-entries-function)
    (:mode-init-function    mode-initialize-function)
    (:message-function      message-function)
    (:buffer-name           buffer-name t)
    (:titles                titles always)
    (:history-size          history-size t)
    (:revert-confirm?       revert-confirm t))
  "Specifications for generating interface variables.
Each specification has the following form:

  (KEYWORD SYMBOL-SUFFIX [GENERATE])

KEYWORD is what can be specified in `bui-define-interface' macro.

SYMBOL-SUFFIX defines the name of a generated variable (it is
prefixed with ENTRY-TYPE-BUFFER-TYPE).

If GENERATE is nil, generate the variable only if a keyword/value
pair is specified in the macro.  If it is t, generate the
variable, unless the defined interface is reduced.  If it is a
symbol `always', generate the variable even for the reduced
interface.")

(defconst bui-all-symbol-specifications
  (append bui-entry-symbol-specifications
          bui-symbol-specifications))

(defalias 'bui-symbol-specification-keyword #'cl-first
  "Return keyword from symbol specification.")

(defalias 'bui-symbol-specification-suffix #'cl-second
  "Return symbol suffix from symbol specification.")

(defalias 'bui-symbol-specification-generate #'cl-third
  "Return 'generate' value from symbol specification.")

(defun bui-symbol-generate? (generate &optional reduced?)
  "Return non-nil if a symbol should be generated.
See `bui-symbol-specifications' for the meaning of GENERATE.
If REDUCED? is non-nil, it means a reduced interface should be defined."
  (or (eq generate 'always)
      (and generate (not reduced?))))

(defun bui-map-symbol-specifications (function specifications)
  "Map through SPECIFICATIONS using FUNCTION.
SPECIFICATIONS should have a form of `bui-symbol-specifications'."
  (mapcar (lambda (spec)
            (funcall function
                     (bui-symbol-specification-keyword spec)
                     (bui-symbol-specification-suffix spec)
                     (bui-symbol-specification-generate spec)))
          specifications))

(defun bui-set-local-variables (entry-type buffer-type suffixes)
  "Set BUI variables according to ENTRY-TYPE/BUFFER-TYPE variables."
  (dolist (suffix suffixes)
    (let ((val (bui-symbol-value entry-type buffer-type suffix)))
      (when val
        (let* ((var (bui-make-symbol 'bui buffer-type suffix))
               (var (if (boundp var)
                        var
                      (bui-make-symbol 'bui suffix))))
          (set (make-local-variable var) val))))))


;;; Wrappers for defined variables

(defalias 'bui-entry-symbol #'bui-make-symbol)
(defalias 'bui-symbol #'bui-make-symbol)

(defun bui-entry-symbol-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE."
  (symbol-value
   (bui-symbol-if-bound (bui-entry-symbol entry-type symbol))))

(defun bui-symbol-value (entry-type buffer-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE/BUFFER-TYPE."
  (or (symbol-value (bui-symbol-if-bound
                     (bui-symbol entry-type buffer-type symbol)))
      (bui-entry-symbol-value entry-type symbol)))

(defun bui-get-entries (entry-type buffer-type args)
  "Return ENTRY-TYPE entries.
Call an appropriate 'get-entries' function using ARGS as its arguments."
  (apply (bui-symbol-value entry-type buffer-type 'get-entries-function)
         args))

(defun bui-enable-mode (entry-type buffer-type)
  "Turn on major mode to display ENTRY-TYPE ENTRIES in BUFFER-TYPE buffer."
  (funcall (bui-symbol entry-type buffer-type 'mode)))

(defun bui-initialize-mode-default (entry-type buffer-type)
  "Default function to set up BUFFER-TYPE buffer for ENTRY-TYPE entries."
  (setq-local revert-buffer-function 'bui-revert)
  (bui-set-local-variables entry-type buffer-type
                           (mapcar #'bui-symbol-specification-suffix
                                   bui-all-symbol-specifications))
  (funcall (bui-make-symbol 'bui buffer-type 'mode-initialize)
           entry-type))

(defun bui-initialize-mode (entry-type buffer-type)
  "Set up the current BUFFER-TYPE buffer to display ENTRY-TYPE entries."
  (--if-let (bui-symbol-value entry-type buffer-type
                              'mode-initialize-function)
      (funcall it)
    (bui-initialize-mode-default entry-type buffer-type)))

(defun bui-insert-entries (entries entry-type buffer-type)
  "Show ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (funcall (bui-make-symbol 'bui buffer-type 'insert-entries)
           entries entry-type))

(defun bui-show-entries-default (entries entry-type buffer-type)
  "Default function to show ENTRY-TYPE ENTRIES in the BUFFER-TYPE buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (bui-enable-mode entry-type buffer-type)
    (let ((filtered-entries (apply #'bui-filter
                                   entries bui-active-filter-predicates)))
      (if filtered-entries
          (bui-insert-entries filtered-entries entry-type buffer-type)
        (message (substitute-command-keys
                  "Everything is filtered out :-)
Use '\\[bui-disable-filters]' to remove filters"))))
    (goto-char (point-min))))

(defun bui-show-entries (entries entry-type buffer-type)
  "Show ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (--if-let (bui-symbol-value entry-type buffer-type
                              'show-entries-function)
      (funcall it entries)
    (bui-show-entries-default entries entry-type buffer-type)))

(defun bui-message (entries entry-type buffer-type args)
  "Display a message for BUFFER-ITEM after showing entries."
  (--when-let (bui-symbol-value entry-type buffer-type
                                'message-function)
    (apply it entries args)))

(defun bui-buffer-name (entry-type buffer-type args)
  "Return name of BUFFER-TYPE buffer for displaying ENTRY-TYPE entries."
  (let ((val (bui-symbol-value entry-type buffer-type 'buffer-name)))
    (cond
     ((stringp val)
      val)
     ((functionp val)
      (apply val args))
     (t
      (concat "*"
              (capitalize (symbol-name entry-type))
              " "
              (capitalize (symbol-name buffer-type))
              "*")))))

(defun bui-param-title (entry-type buffer-type param)
  "Return PARAM title for ENTRY-TYPE/BUFFER-TYPE."
  (or (bui-assq-value (bui-symbol-value entry-type buffer-type 'titles)
                      param)
      (bui-assq-value (bui-entry-symbol-value entry-type 'titles)
                      param)
      (bui-symbol-title param)))

(defun bui-available-filter-predicates (entry-type buffer-type)
  "Return available filter predicates for ENTRY-TYPE/BUFFER-TYPE."
  (bui-symbol-value entry-type buffer-type 'filter-predicates))

(defun bui-boolean-param? (entry-type buffer-type param)
  "Return non-nil if PARAM for ENTRY-TYPE/BUFFER-TYPE is boolean."
  (memq param (bui-symbol-value entry-type buffer-type 'boolean-params)))

(defun bui-history-size (entry-type buffer-type)
  "Return history size for ENTRY-TYPE/BUFFER-TYPE."
  (bui-symbol-value entry-type buffer-type 'history-size))

(defun bui-revert-confirm? (entry-type buffer-type)
  "Return 'revert-confirm' value for ENTRY-TYPE/BUFFER-TYPE."
  (bui-symbol-value entry-type buffer-type 'revert-confirm))


;;; Displaying entries

(defun bui-display (buffer)
  "Switch to a BUI BUFFER."
  (pop-to-buffer buffer
                 '((display-buffer-reuse-window
                    display-buffer-same-window))))

(defun bui-history-item (buffer-item)
  "Make and return a history item for displaying BUFFER-ITEM."
  (list #'bui-set buffer-item 'no))

(defun bui-set (buffer-item &optional history)
  "Set up the current buffer for displaying BUFFER-ITEM.
HISTORY should be one of the following:

  `nil' or `add' - add it to history,

  `no' - do not save BUFFER-ITEM in history,

  `replace' - replace the current history item."
  (bui-with-item buffer-item
    (when %entries
      ;; Set buffer item before showing entries, so that its value can
      ;; be used by the code for displaying entries.
      (setq bui-item buffer-item)
      (bui-show-entries %entries %entry-type %buffer-type)
      (unless (eq history 'no)
        (funcall (cl-ecase history
                   ((nil add) #'bui-history-add)
                   (replace   #'bui-history-replace))
                 (bui-history-item buffer-item))))
    (bui-message %entries %entry-type %buffer-type %args)))

(defun bui-display-entries-current (entries entry-type buffer-type args
                                    &optional history)
  "Show ENTRIES in the current BUI buffer.
See `bui-item' for the meaning of BUFFER-TYPE, ENTRY-TYPE
and ARGS, and `bui-set' for the meaning of HISTORY."
  (bui-set (bui-make-item entries entry-type buffer-type args)
           history))

(defun bui-get-display-entries-current (entry-type buffer-type args
                                        &optional history)
  "Search for entries and show them in the current BUI buffer.
See `bui-display-entries-current' for details."
  (bui-display-entries-current
   (bui-get-entries entry-type buffer-type args)
   entry-type buffer-type args history))

(defun bui-display-entries (entries entry-type buffer-type args
                            &optional history)
  "Show ENTRIES in a BUFFER-TYPE buffer.
See `bui-display-entries-current' for details."
  (let ((buffer (get-buffer-create
                 (bui-buffer-name entry-type buffer-type args))))
    (with-current-buffer buffer
      (bui-display-entries-current
       entries entry-type buffer-type args history))
    (when entries
      (bui-display buffer))))

(defun bui-get-display-entries (entry-type buffer-type args
                                &optional history)
  "Search for entries and show them in a BUFFER-TYPE buffer.
See `bui-display-entries-current' for details."
  (bui-display-entries
   (bui-get-entries entry-type buffer-type args)
   entry-type buffer-type args history))

(defun bui-revert (_ignore-auto noconfirm)
  "Update the data in the current BUI buffer.
This function is suitable for `revert-buffer-function'.
See `revert-buffer' for the meaning of NOCONFIRM."
  (bui-with-current-item
    (ignore %entries)           ; to avoid compilation warning
    (when (or noconfirm
              (not (bui-revert-confirm? %entry-type %buffer-type))
              (y-or-n-p "Update the current buffer? "))
      (bui-get-display-entries-current
       %entry-type %buffer-type %args 'replace))))

(defvar bui-after-redisplay-hook nil
  "Hook run by `bui-redisplay'.
This hook is called before seting up a window position.")

(defun bui-redisplay ()
  "Redisplay the current BUI buffer.
Restore the point and window positions after redisplaying.

This function does not update the buffer data, use
'\\[revert-buffer]' if you want the full update."
  (interactive)
  (let* ((old-point (point))
         ;; For simplicity, ignore an unlikely case when multiple
         ;; windows display the same buffer.
         (window (car (get-buffer-window-list (current-buffer) nil t)))
         (window-start (and window (window-start window))))
    (bui-set bui-item 'no)
    (goto-char old-point)
    (run-hooks 'bui-after-redisplay-hook)
    (when window
      (set-window-point window (point))
      (set-window-start window window-start))))

(defun bui-redisplay-goto-button ()
  "Redisplay the current buffer and go to the next button, if needed."
  (let ((bui-after-redisplay-hook
         (cons (lambda ()
                 (unless (button-at (point))
                   (forward-button 1)))
               bui-after-redisplay-hook)))
    (bui-redisplay)))


;;; Interface definers

(defmacro bui-define-entry-type (entry-type &rest args)
  "Define variables for ENTRY-TYPE.
ARGS can be the same arguments as for `bui-define-interface'.
The difference is: arguments for `bui-define-interface' define
specific variables for different buffer types, while this macro
defines general variables used for any buffer type."
  (declare (indent 1))
  (bui-plist-let args
      ((reduced? :reduced?))
    `(progn
       ,@(bui-map-symbol-specifications
          (lambda (key suffix generate)
            (let ((val (plist-get %foreign-args key)))
              (when (or val (bui-symbol-generate? generate reduced?))
                (bui-inherit-defvar-clause
                 (bui-entry-symbol entry-type suffix)
                 (bui-make-symbol 'bui suffix)
                 :value val
                 :group entry-type))))
          bui-entry-symbol-specifications)

       ,@(bui-map-symbol-specifications
          (lambda (key suffix _generate)
            (let ((val (plist-get %foreign-args key)))
              (when val
                (bui-inherit-defvar-clause
                 (bui-entry-symbol entry-type suffix)
                 (bui-make-symbol 'bui suffix)
                 :value val
                 :group entry-type))))
          bui-symbol-specifications))))

(defmacro bui-define-interface (entry-type buffer-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE entries.
Remaining arguments ARGS should have a form [KEYWORD VALUE] ...
They are used to generate variables specific for the defined
interface.  For more details and the available keywords, see
`bui-symbol-specifications' and `bui-entry-symbol-specifications'.

`:get-entries-function' is the only required keyword (if the
interface is reduced, all keywords become optional).

To denote that the interface is reduced, a special `:reduced?'
keyword may be specified.  If it is non-nil, generate only
customization group, faces group and specified variables.  If it
is nil, along with the mentioned groups and variables,
`ENTRY-TYPE-BUFFER-TYPE-mode' will be generated."
  (declare (indent 2))
  (cl-flet ((name (&rest symbols)
              (apply #'bui-symbol entry-type buffer-type symbols))
            (bui-name (&rest symbols)
              (apply #'bui-make-symbol 'bui symbols)))
    (let ((group              (name))
          (faces-group        (name 'faces))
          (mode               (name 'mode))
          (mode-map           (name 'mode-map))
          (bui-buffer-type    (bui-name buffer-type))
          (parent-mode        (bui-name buffer-type 'mode)))
      (bui-plist-let args
          ((mode-name         :mode-name (capitalize (symbol-name group)))
           (reduced?          :reduced?))
        `(progn
           (defgroup ,group nil
             ,(format "Displaying '%S' entries in '%S' buffer."
                      entry-type buffer-type)
             :group ',entry-type
             :group ',bui-buffer-type)

           (defgroup ,faces-group nil
             ,(format "Faces for displaying '%S' entries in '%S' buffer."
                      entry-type buffer-type)
             :group ',group
             :group ',(bui-name entry-type 'faces)
             :group ',(bui-name buffer-type 'faces))

           ,@(bui-map-symbol-specifications
              (lambda (key suffix generate)
                (let ((val (plist-get %foreign-args key)))
                  (when (or val (bui-symbol-generate? generate reduced?))
                    (bui-inherit-defvar-clause
                     (name suffix)
                     (bui-name suffix)
                     :value val
                     :group group))))
              bui-symbol-specifications)

           ,@(bui-map-symbol-specifications
              (lambda (key suffix _generate)
                (let ((val (plist-get %foreign-args key)))
                  (when val
                    (bui-inherit-defvar-clause
                     (name suffix)
                     (bui-name suffix)
                     :value val
                     :group group))))
              bui-entry-symbol-specifications)

           ,(unless reduced?
              `(define-derived-mode ,mode ,parent-mode
                 '(,mode-name (bui-active-filter-predicates
                               bui-filter-mode-line-string))
                 ,(format "\
Major mode for displaying '%S' entries in '%S' buffer.

\\{%S}"
                          entry-type buffer-type mode-map)
                 (bui-initialize-mode ',entry-type ',buffer-type))))))))


(defvar bui-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group (or "bui-with-item"
                           "bui-with-current-item"
                           "bui-define-entry-type"
                           "bui-define-interface"))
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode bui-font-lock-keywords)

(provide 'bui)

;;; bui.el ends here
