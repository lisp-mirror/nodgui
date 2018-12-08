;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018 cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :nodgui)

(defargs entry ()
  class
  cursor
  exportselection
  invalidcommand
  justify
  show
  state
  style
  takefocus
  textvariable
  validate
  validatecommand
  width
  xscrollcommand)


(defwrapper entry (tktextvariable widget) () "ttk::entry")

(defun entry-select (e from to)
  (format-wish "~a selection range ~a ~a" (widget-path e) from to)
  e)

(defgeneric cursor-index (widget)
  (:documentation "returns the cursor index in the widget"))

(defmethod cursor-index ((e entry))
  (format-wish "senddata [~a index insert]" (widget-path e))
  (read-data))
