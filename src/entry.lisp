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

(defun ensure-validation-type-correct (validation-mode)
  (assert (not (member validation-mode '(:all :focus)))
          nil
          ":all validation in not supported, allowed are: :none :key :focusin :focusout"))

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

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

(defwrapper entry (tktextvariable widget) () "ttk::entry"
  (ensure-validation-type-correct validate))

(defun entry-select (e from to)
  (warn "entry-select is deprecated, use 'set-selection' instead")
  (format-wish "~a selection range {~a} {~a}" (widget-path e) from to)
  e)

(defgeneric cursor-index (widget)
  (:documentation "returns the cursor index in the widget"))

(defgeneric set-selection (object from to)
  (:documentation "set the selected text in range 'from', 'to'."))

(defgeneric set-cursor-index (object index)
  (:documentation "set cursor position; index can assume the value ':end'"))

(defgeneric clear-selection (object))

(defmethod cursor-index ((e entry))
  (with-read-data ()
    (format-wish "senddata [~a index insert]" (widget-path e))))

(defmethod set-selection ((object entry) from to)
  (format-wish (tclize `(,(widget-path object)
                          " selection range "
                          ,(down from)  " "
                          ,(down to))))
  object)

(defmethod set-cursor-index ((object entry) index)
  (format-wish (tclize  `(,(widget-path object)
                           " icursor "
                           ,(down index))))
  object)

(defmethod clear-selection ((object entry))
  (format-wish (tclize  `(,(widget-path object) " selection clear ")))
  object)

(defmethod insert-at-cursor ((object entry) text)
  (format-wish (tclize  `(,(widget-path object) " insert insert " {+ ,text })))
  object)
