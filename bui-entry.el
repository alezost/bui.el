;;; bui-entry.el --- 'Entry' type  -*- lexical-binding: t -*-

;; Copyright © 2015-2016 Alex Kost <alezost@gmail.com>

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

;; This file provides an API for 'entry' type which is just an alist of
;; KEY/VALUE pairs (KEY should be a symbol) with the required 'id' KEY.

;;; Code:

(require 'dash)
(require 'bui-utils)

(defalias 'bui-entry-value #'bui-assq-value)

(defun bui-entry-id (entry)
  "Return ENTRY ID."
  (bui-entry-value entry 'id))

(defun bui-entry-by-id (id entries)
  "Return an entry from ENTRIES by its ID."
  (--find (equal (bui-entry-id it) id)
          entries))

(defun bui-entries-by-ids (ids entries)
  "Return entries with IDS (a list of identifiers) from ENTRIES."
  (--filter (member (bui-entry-id it) ids)
            entries))

(defun bui-replace-entry (id new-entry entries)
  "Replace an entry with ID from ENTRIES by NEW-ENTRY.
Return a list of entries with the replaced entry."
  (--map-first (equal id (bui-entry-id it))
               new-entry
               entries))

(provide 'bui-entry)

;;; bui-entry.el ends here
