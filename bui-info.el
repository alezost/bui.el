;;; bui-info.el --- 'Info' buffer interface for displaying data  -*- lexical-binding: t -*-

;; Copyright © 2014-2016 Alex Kost <alezost@gmail.com>
;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>

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

;; This file provides 'info' (help-like) buffer interface for displaying
;; an arbitrary data.

;;; Code:

(require 'cus-edit)             ; for button faces
(require 'dash)
(require 'bui)
(require 'bui-entry)
(require 'bui-utils)

(bui-define-groups bui-info)

(defface bui-info-heading
  '((((type tty pc) (class color)) :weight bold)
    (t :inherit variable-pitch :height 1.4 :weight bold))
  "Face for headings."
  :group 'bui-info-faces)

(defface bui-info-param-title
  '((t :inherit font-lock-type-face))
  "Face used for titles of parameters."
  :group 'bui-info-faces)

(defface bui-info-file-name
  '((t :inherit link))
  "Face used for file names."
  :group 'bui-info-faces)

(defface bui-info-url
  '((t :inherit link))
  "Face used for URLs."
  :group 'bui-info-faces)

(defface bui-info-time
  '((t :inherit font-lock-constant-face))
  "Face used for timestamps."
  :group 'bui-info-faces)

(defface bui-info-action-button
  '((t :inherit custom-button))
  "Face used for action buttons."
  :group 'bui-info-faces)

(defface bui-info-action-button-mouse
  '((t :inherit custom-button-mouse))
  "Mouse face used for action buttons."
  :group 'bui-info-faces)


;;; General 'info' variables

(defvar bui-info-format nil
  "List of methods for inserting entries.
Each METHOD should be either nil, a function or a list.

If METHOD is nil, newline is inserted at point.

If METHOD is a function, it is called with an entry as argument.

If METHOD is a list, it should have the following form:

  (PARAM INSERT-TITLE INSERT-VALUE)

PARAM is a name of the entry parameter.

INSERT-TITLE may be either a symbol or a list.  If it is a
symbol, it should be a function or an alias from
`bui-info-title-aliases', in which case it is called with title
as argument.  If it is a list, it should have a
form (FUN-OR-ALIAS [ARGS ...]), in which case FUN-OR-ALIAS is
called with title and ARGS as arguments.

INSERT-VALUE may be either a symbol or a list.  If it is a
symbol, it should be a function or an alias from
`bui-info-value-aliases', in which case it is called with value
and entry as arguments.  If it is a list, it should have a
form (FUN-OR-ALIAS [ARGS ...]), in which case FUN-OR-ALIAS is
called with value and ARGS as arguments.

After inserting title/value with such a list METHOD, a new line
is inserted.

Parameters are inserted in the same order as defined by this list.")

(defcustom bui-info-ignore-empty-values nil
  "If non-nil, do not display non-boolean parameters with nil values."
  :type 'boolean
  :group 'bui-info)

(defcustom bui-info-ignore-void-values t
  "If non-nil, do not display non-existing parameters."
  :type 'boolean
  :group 'bui-info)

(defcustom bui-info-fill t
  "If non-nil, fill string parameters to fit the window.
If nil, insert text parameters in a raw form."
  :type 'boolean
  :group 'bui-info)

(defcustom bui-info-param-title-format "%-18s: "
  "String used to format a title of a parameter.
It should be a '%s'-sequence.  After inserting a title formatted
with this string, a value of the parameter is inserted.
This string is used by `bui-info-insert-title-format'."
  :type 'string
  :group 'bui-info)

(defcustom bui-info-multiline-prefix
  (make-string (length (format bui-info-param-title-format " "))
               ?\s)
  "String used to format multi-line parameter values.
If a value occupies more than one line, this string is inserted
in the beginning of each line after the first one.
This string is used by `bui-info-insert-value-format'."
  :type 'string
  :group 'bui-info)

(defcustom bui-info-indent 2
  "Number of spaces used to indent various parts of inserted text."
  :type 'integer
  :group 'bui-info)

(defcustom bui-info-delimiter "\n\f\n"
  "String used to separate entries."
  :type 'string
  :group 'bui-info)

(defconst bui-info-symbol-specifications
  '((:delimiter delimiter t)
    (:fill fill t)
    (:format format always)
    (:ignore-empty-values ignore-empty-values t)
    (:ignore-void-values ignore-void-values t)
    (:indent indent t)
    (:multiline-prefix multiline-prefix t)
    (:title-format param-title-format t))
  "Specifications for generating 'info' variables.
See `bui-symbol-specifications' for details.")


;;; Wrappers for 'info' variables

(defun bui-info-symbol (entry-type symbol)
  "Return symbol for ENTRY-TYPE and 'info' buffer type."
  (bui-symbol entry-type 'info symbol))

(defun bui-info-symbol-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE and 'info' buffer type."
  (bui-symbol-value entry-type 'info symbol))

(defun bui-info-param-title (entry-type param)
  "Return a title of an ENTRY-TYPE parameter PARAM."
  (bui-param-title entry-type 'info param))

(defun bui-info-format (entry-type)
  "Return 'info' format for ENTRY-TYPE."
  (bui-info-symbol-value entry-type 'format))

(defun bui-info-displayed-params (entry-type)
  "Return a list of ENTRY-TYPE parameters that should be displayed."
  (-non-nil
   (--map (pcase it
            (`(,param . ,_) param))
          (bui-info-format entry-type))))


;;; Inserting entries

(defvar bui-info-title-aliases
  '((format . bui-info-insert-title-format)
    (simple . bui-info-insert-title-simple))
  "Alist of aliases and functions to insert titles.")

(defvar bui-info-value-aliases
  '((format . bui-info-insert-value-format)
    (indent . bui-info-insert-value-indent)
    (simple . bui-info-insert-value-simple)
    (time   . bui-info-insert-time))
  "Alist of aliases and functions to insert values.")

(defun bui-info-title-function (fun-or-alias)
  "Convert FUN-OR-ALIAS into a function to insert a title."
  (or (bui-assq-value bui-info-title-aliases fun-or-alias)
      fun-or-alias))

(defun bui-info-value-function (fun-or-alias)
  "Convert FUN-OR-ALIAS into a function to insert a value."
  (or (bui-assq-value bui-info-value-aliases fun-or-alias)
      fun-or-alias))

(defun bui-info-title-method->function (method)
  "Convert title METHOD into a function to insert a title."
  (pcase method
    ((pred null) #'ignore)
    ((pred symbolp) (bui-info-title-function method))
    (`(,fun-or-alias . ,rest-args)
     (lambda (title)
       (apply (bui-info-title-function fun-or-alias)
              title rest-args)))
    (_ (error "Unknown title method '%S'" method))))

(defun bui-info-value-method->function (method)
  "Convert value METHOD into a function to insert a value."
  (pcase method
    ((pred null) #'ignore)
    ((pred functionp) method)
    (`(,fun-or-alias . ,rest-args)
     (lambda (value _)
       (apply (bui-info-value-function fun-or-alias)
              value rest-args)))
    (_ (error "Unknown value method '%S'" method))))

(defun bui-info-fill-column ()
  "Return fill column for the current window."
  (min (window-width) fill-column))

(defun bui-info-get-indent (&optional level)
  "Return `bui-info-indent' \"multiplied\" by LEVEL spaces.
LEVEL is 1 by default."
  (make-string (* bui-info-indent (or level 1)) ?\s))

(defun bui-info-insert-indent (&optional level)
  "Insert `bui-info-indent' spaces LEVEL times (1 by default)."
  (insert (bui-info-get-indent level)))

(defun bui-info-insert-entries (entries entry-type)
  "Display ENTRY-TYPE ENTRIES in the current info buffer."
  (bui-mapinsert (lambda (entry)
                   (bui-info-insert-entry entry entry-type))
                 entries
                 bui-info-delimiter))

(defun bui-info-insert-entry (entry entry-type &optional indent-level)
  "Insert ENTRY-TYPE ENTRY into the current info buffer.
If INDENT-LEVEL is non-nil, indent displayed data by this number
of `bui-info-indent' spaces."
  (bui-with-indent (* (or indent-level 0)
                      bui-info-indent)
    (dolist (spec (bui-info-format entry-type))
      (bui-info-insert-entry-unit spec entry entry-type))))

(defun bui-info-insert-entry-unit (format-spec entry entry-type)
  "Insert title and value of a PARAM at point.
ENTRY is alist with parameters and their values.
ENTRY-TYPE is a type of ENTRY."
  (pcase format-spec
    ((pred null)
     (bui-newline))
    ((pred functionp)
     (funcall format-spec entry))
    (`(,param ,title-method ,value-method)
     (let* ((value    (bui-entry-value entry param))
            (void?    (bui-void-value? value))
            (empty?   (null value))
            (boolean? (bui-boolean-param? entry-type 'info param)))
       (unless (or (and bui-info-ignore-void-values void?)
                   (and bui-info-ignore-empty-values
                        empty? (not boolean?)))
         (let ((title        (bui-info-param-title entry-type param))
               (insert-title (bui-info-title-method->function title-method))
               (insert-value (bui-info-value-method->function value-method)))
           (funcall insert-title title)
           (cond
            (void? (insert bui-empty-string))
            ((and empty? boolean?) (insert bui-false-string))
            (t (funcall insert-value value entry)))
           (bui-newline)))))
    (_ (error "Unknown format specification '%S'" format-spec))))

(defun bui-info-insert-title-simple (title &optional face)
  "Insert \"TITLE: \" string at point.
If FACE is nil, use `bui-info-param-title'."
  (bui-format-insert title
                     (or face 'bui-info-param-title)
                     "%s: "))

(defun bui-info-insert-title-format (title &optional face)
  "Insert TITLE using `bui-info-param-title-format' at point.
If FACE is nil, use `bui-info-param-title'."
  (bui-format-insert title
                     (or face 'bui-info-param-title)
                     bui-info-param-title-format))

(defun bui-info-insert-value-simple (value &optional button-or-face indent)
  "Format and insert parameter VALUE at point.

VALUE may be split into several short lines to fit the current
window, depending on `bui-info-fill', and each line is indented
with INDENT number of spaces.

If BUTTON-OR-FACE is a button type symbol, transform VALUE into
this (these) button(s) and insert each one on a new line.  If it
is a face symbol, propertize inserted line(s) with this face."
  (or indent (setq indent 0))
  (bui-with-indent indent
    (let* ((button?  (bui-button-type? button-or-face))
           (face     (unless button? button-or-face))
           (fill-col (unless (or button?
                                 (and (stringp value)
                                      (not bui-info-fill)))
                       (- (bui-info-fill-column) indent)))
           (value    (if (and value button?)
                         (bui-buttonize value button-or-face "\n")
                       value)))
      (bui-split-insert value face fill-col "\n"))))

(defun bui-info-insert-value-indent (value &optional button-or-face)
  "Format and insert parameter VALUE at point.

This function is intended to be called after inserting a title
with `bui-info-insert-title-simple'.

VALUE may be split into several short lines to fit the current
window, depending on `bui-info-fill', and each line is indented
with `bui-info-indent'.

For the meaning of BUTTON-OR-FACE, see `bui-info-insert-value-simple'."
  (when value (bui-newline))
  (bui-info-insert-value-simple value button-or-face bui-info-indent))

(defun bui-info-insert-value-format (value &optional button-or-face
                                           &rest button-properties)
  "Format and insert parameter VALUE at point.

This function is intended to be called after inserting a title
with `bui-info-insert-title-format'.

VALUE may be split into several short lines to fit the current
window, depending on `bui-info-fill' and
`bui-info-multiline-prefix'.  If VALUE is a list, its elements
will be separated with `bui-list-separator'.

If BUTTON-OR-FACE is a button type symbol, transform VALUE into
this (these) button(s).  If it is a face symbol, propertize
inserted line(s) with this face.

BUTTON-PROPERTIES are passed to `bui-buttonize' (only if
BUTTON-OR-FACE is a button type)."
  (let* ((button?  (bui-button-type? button-or-face))
         (face     (unless button? button-or-face))
         (fill-col (when (or button?
                             bui-info-fill
                             (not (stringp value)))
                     (- (bui-info-fill-column)
                        (length bui-info-multiline-prefix))))
         (value    (if (and value button?)
                       (apply #'bui-buttonize
                              value button-or-face bui-list-separator
                              button-properties)
                     value)))
    (bui-split-insert value face fill-col
                      (concat "\n" bui-info-multiline-prefix))))

(defun bui-info-insert-time (time &optional face)
  "Insert formatted time string using TIME at point.
See `bui-get-time-string' for the meaning of TIME."
  (bui-format-insert (bui-get-time-string time)
                     (or face 'bui-info-time)))


;;; Buttons

(defvar bui-info-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map (kbd "c") 'bui-info-button-copy-label)
    map)
  "Keymap for buttons in info buffers.")

(define-button-type 'bui
  'keymap bui-info-button-map
  'follow-link t)

(define-button-type 'bui-action
  :supertype 'bui
  'face 'bui-info-action-button
  'mouse-face 'bui-info-action-button-mouse)

(define-button-type 'bui-file
  :supertype 'bui
  'face 'bui-info-file-name
  'help-echo "Find file"
  'action (lambda (btn)
            (bui-find-file (button-label btn))))

(define-button-type 'bui-url
  :supertype 'bui
  'face 'bui-info-url
  'help-echo "Browse URL"
  'action (lambda (btn)
            (browse-url (button-label btn))))

(defun bui-info-button-copy-label (&optional pos)
  "Copy a label of the button at POS into kill ring.
If POS is nil, use the current point position."
  (interactive)
  (--when-let (button-at (or pos (point)))
    (bui-copy-as-kill (button-label it))))

(defun bui-info-insert-action-button (label action &optional message
                                      &rest properties)
  "Make action button with LABEL and insert it at point.
ACTION is a function called when the button is pressed.  It
should accept button as the argument.
MESSAGE is a button message.
See `insert-text-button' for the meaning of PROPERTIES."
  (apply #'bui-insert-button
         label 'bui-action
         'action action
         'help-echo message
         properties))


;;; Major mode and interface definer

(defvar bui-info-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent
     map (make-composed-keymap (list bui-map button-buffer-map)
                               special-mode-map))
    map)
  "Keymap for `bui-info-mode' buffers.")

(define-derived-mode bui-info-mode special-mode "BUI-Info"
  "Parent mode for displaying data in 'info' form.")

(defun bui-info-mode-initialize (entry-type)
  "Set up the current 'info' buffer."
  (bui-set-local-variables entry-type 'info
                           (mapcar #'bui-symbol-specification-suffix
                                   bui-info-symbol-specifications))
  ;; Without this, syntactic fontification is performed, and it may
  ;; break highlighting.  For example, if there is a single "
  ;; (double-quote) character, the default syntactic fontification
  ;; highlights the rest text after it as a string.
  ;; See (info "(elisp) Font Lock Basics") for details.
  (setq font-lock-defaults '(nil t)))

(defmacro bui-define-info-interface (entry-type &rest args)
  "Define 'info' interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...
They are used to generate variables specific for the defined info
interface.  For more details and the available keywords, see
`bui-info-symbol-specifications'.

The rest keyword arguments are passed to
`bui-define-interface' macro."
  (declare (indent 1))
  (let* ((entry-type-str     (symbol-name entry-type))
         (prefix             (concat entry-type-str "-info"))
         (group              (intern prefix)))
    (bui-plist-let args
        ((reduced?           :reduced?))
      `(progn
         ,@(bui-map-symbol-specifications
            (lambda (key suffix generate)
              (let ((val (plist-get args key)))
                (when (or val (bui-symbol-generate? generate reduced?))
                  (bui-inherit-defvar-clause
                   (bui-info-symbol entry-type suffix)
                   (bui-make-symbol 'bui-info suffix)
                   :value val
                   :group group))))
            bui-info-symbol-specifications)

         (bui-define-interface ,entry-type info
           ,@args)))))


(defvar bui-info-font-lock-keywords
  (eval-when-compile
    `((,(rx "(" (group "bui-define-info-interface")
            symbol-end)
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode bui-info-font-lock-keywords)

(provide 'bui-info)

;;; bui-info.el ends here
