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

(require 'dash)
(require 'bui-history)
(require 'bui-utils)

(bui-define-groups bui
  :parent-group external
  :parent-faces-group faces
  :group-doc "Settings for Buffer User Interface.")

(defvar bui-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'bui-history-back)
    (define-key map (kbd "r") 'bui-history-forward)
    (define-key map (kbd "g") 'revert-buffer)
    (define-key map (kbd "R") 'bui-redisplay)
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


;;; Wrappers for defined variables

(defalias 'bui-entry-symbol #'bui-make-symbol)
(defalias 'bui-symbol #'bui-make-symbol)

(defun bui-entry-symbol-value (entry-type symbol)
  "Return SYMBOL's value for ENTRY-TYPE."
  (symbol-value
   (bui-symbol-if-bound (bui-entry-symbol entry-type symbol))))

(defun bui-symbol-value (entry-type buffer-type symbol)
  "Return SYMBOL's value for BUFFER-TYPE/ENTRY-TYPE."
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
  (funcall (bui-symbol-value entry-type buffer-type 'mode-function)))

(defun bui-initialize-mode-default (entry-type buffer-type)
  "Default function to set up BUFFER-TYPE buffer for ENTRY-TYPE entries."
  (funcall (bui-make-symbol 'bui buffer-type 'mode-initialize)
           entry-type))

(defun bui-initialize-mode (entry-type buffer-type)
  "Set up the current BUFFER-TYPE buffer to display ENTRY-TYPE entries."
  (funcall (bui-symbol-value entry-type buffer-type
                             'mode-initialize-function)))

(defun bui-insert-entries (entries entry-type buffer-type)
  "Show ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (funcall (bui-make-symbol 'bui buffer-type 'insert-entries)
           entries entry-type))

(defun bui-show-entries-default (entries entry-type buffer-type)
  "Default function to show ENTRY-TYPE ENTRIES in the BUFFER-TYPE buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (bui-enable-mode entry-type buffer-type)
    (bui-insert-entries entries entry-type buffer-type)
    (goto-char (point-min))))

(defun bui-show-entries (entries entry-type buffer-type)
  "Show ENTRY-TYPE ENTRIES in the current BUFFER-TYPE buffer."
  (funcall (bui-symbol-value entry-type buffer-type
                             'show-entries-function)
           entries))

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
  "Return PARAM title for BUFFER-TYPE/ENTRY-TYPE."
  (or (bui-assq-value (bui-symbol-value entry-type buffer-type 'titles)
                      param)
      (bui-assq-value (bui-entry-symbol-value entry-type 'titles)
                      param)
      (bui-symbol-title param)))

(defun bui-history-size (entry-type buffer-type)
  "Return history size for BUFFER-TYPE/ENTRY-TYPE."
  (bui-symbol-value entry-type buffer-type 'history-size))

(defun bui-revert-confirm? (entry-type buffer-type)
  "Return 'revert-confirm' value for BUFFER-TYPE/ENTRY-TYPE."
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
  `(progn
     ,@(bui-map-plist
        (lambda (key val)
          `(defvar ,(bui-make-symbol entry-type
                                     (bui-keyword->symbol key))
             ,val))
        args)))

(defmacro bui-define-interface (entry-type buffer-type &rest args)
  "Define BUFFER-TYPE interface for displaying ENTRY-TYPE entries.
Remaining arguments (ARGS) should have a form [KEYWORD VALUE] ...
In the following description TYPE means ENTRY-TYPE-BUFFER-TYPE.

Required keywords:

  - `:get-entries-function' - default value of the generated
    `TYPE-get-entries-function' variable.

Optional keywords:

  - `:show-entries-function' - default value of the generated
    `TYPE-show-entries-function' variable.

  - `:buffer-name' - default value of the generated
    `TYPE-buffer-name' variable.

  - `:message-function' - default value of the generated
    `TYPE-message-function' variable.

  - `:titles' - default value of the generated
    `TYPE-titles' variable.

  - `:history-size' - default value of the generated
    `TYPE-history-size' variable.

  - `:revert-confirm?' - default value of the generated
    `TYPE-revert-confirm' variable.

  - `:mode-name' - name (a string appeared in the mode-line) of
     the generated `TYPE-mode'.

  - `:mode-init-function' - default value of the generated
    `TYPE-mode-initialize-function' variable.

  - `:reduced?' - if non-nil, generate only group, faces group
    and titles variable (if specified); all keywords become
    optional."
  (declare (indent 2))
  (let* ((entry-type-str     (symbol-name entry-type))
         (buffer-type-str    (symbol-name buffer-type))
         (prefix             (concat entry-type-str "-" buffer-type-str))
         (group              (intern prefix))
         (faces-group        (intern (concat prefix "-faces")))
         (get-entries-var    (intern (concat prefix "-get-entries-function")))
         (show-entries-var   (intern (concat prefix "-show-entries-function")))
         (show-entries-fun   (intern (concat prefix "-show-entries")))
         (mode-str           (concat prefix "-mode"))
         (mode-map-str       (concat mode-str "-map"))
         (mode               (intern mode-str))
         (parent-mode        (intern (concat "bui-" buffer-type-str "-mode")))
         (mode-var           (intern (concat mode-str "-function")))
         (mode-init-var      (intern (concat mode-str "-initialize-function")))
         (mode-init-fun      (intern (concat prefix "-mode-initialize")))
         (message-var        (intern (concat prefix "-message-function")))
         (buffer-name-var    (intern (concat prefix "-buffer-name")))
         (titles-var         (intern (concat prefix "-titles")))
         (history-size-var   (intern (concat prefix "-history-size")))
         (revert-confirm-var (intern (concat prefix "-revert-confirm"))))
    (bui-plist-let args
        ((get-entries-val    :get-entries-function)
         (show-entries-val   :show-entries-function)
         (mode-name          :mode-name (capitalize prefix))
         (mode-init-val      :mode-init-function)
         (message-val        :message-function)
         (buffer-name-val    :buffer-name)
         (titles-val         :titles)
         (history-size-val   :history-size 20)
         (revert-confirm-val :revert-confirm? t)
         (reduced?           :reduced?))
      `(progn
         (defgroup ,group nil
           ,(format "Displaying '%s' entries in '%s' buffer."
                    entry-type-str buffer-type-str)
           :group ',(intern entry-type-str)
           :group ',(intern (concat "bui-" buffer-type-str)))

         (defgroup ,faces-group nil
           ,(format "Faces for displaying '%s' entries in '%s' buffer."
                    entry-type-str buffer-type-str)
           :group ',group
           :group ',(intern (concat entry-type-str "-faces"))
           :group ',(intern (concat "bui-" buffer-type-str "-faces")))

         (defcustom ,titles-var ,titles-val
           ,(format "Alist of titles of '%s' parameters."
                    entry-type-str)
           :type '(alist :key-type symbol :value-type string)
           :group ',group)

         ,(unless reduced?
            `(progn
               (defvar ,get-entries-var ,get-entries-val
                 ,(format "\
Function used to receive '%s' entries for '%s' buffer."
                          entry-type-str buffer-type-str))

               (defvar ,show-entries-var
                 ,(or show-entries-val `',show-entries-fun)
                 ,(format "\
Function used to show '%s' entries in '%s' buffer."
                          entry-type-str buffer-type-str))

               ,(unless show-entries-val
                  `(defun ,show-entries-fun (entries)
                     ,(format "\
Wrapper for `bui-show-entries-default'.
Call it with '%s' ENTRY-TYPE and '%s' BUFFER-TYPE."
                              entry-type-str buffer-type-str)
                     (bui-show-entries-default
                      entries ',entry-type ',buffer-type)))

               (defvar ,message-var ,message-val
                 ,(format "\
Function used to display a message after showing '%s' entries.
If nil, do not display messages."
                          entry-type-str))

               (defcustom ,buffer-name-var ,buffer-name-val
                 ,(format "\
Default name of '%s' buffer for displaying '%s' entries.
May be nil, a string or a function returning a string.  The
function is called with the same arguments as `%S'.  If nil, the
name is defined automatically."
                          buffer-type-str entry-type-str get-entries-var)
                 :type '(choice string function)
                 :group ',group)

               (defcustom ,history-size-var ,history-size-val
                 ,(format "\
Maximum number of items saved in history of `%S' buffer.
If 0, the history is disabled."
                          buffer-name-var)
                 :type 'integer
                 :group ',group)

               (defcustom ,revert-confirm-var ,revert-confirm-val
                 ,(format "\
If non-nil, ask to confirm for reverting `%S' buffer."
                          buffer-name-var)
                 :type 'boolean
                 :group ',group)

               (defvar ,mode-var ',mode
                  ,(format "\
Major mode for displaying '%s' entries in '%s' buffer."
                           entry-type-str buffer-type-str))

               (defvar ,mode-init-var
                 ,(or mode-init-val `',mode-init-fun)
                 ,(format "\
Function used to set up '%s' buffer for displaying '%s' entries."
                          buffer-type-str entry-type-str))

               ,(unless mode-init-val
                  `(defun ,mode-init-fun ()
                     ,(format "\
Wrapper for `bui-initialize-mode-default'.
Call it with '%s' ENTRY-TYPE and '%s' BUFFER-TYPE."
                              entry-type-str buffer-type-str)
                     (bui-initialize-mode-default
                      ',entry-type ',buffer-type)))

               (define-derived-mode ,mode ,parent-mode ,mode-name
                 ,(format "\
Major mode for displaying '%s' entries in '%s' buffer.

\\{%s}"
                          entry-type-str buffer-type-str mode-map-str)
                 (setq-local revert-buffer-function 'bui-revert)
                 (setq-local bui-history-size
                             (bui-history-size ',entry-type
                                               ',buffer-type))
                 (bui-initialize-mode ',entry-type
                                      ',buffer-type))))))))


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
