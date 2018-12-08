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

(defargs check-button (widget)
  cbcommand
  compound
  image
  offvalue
  onvalue
  state
  textvariable
  underline
  variable
  width)

(defwrapper check-button (tktextvariable widget tkvariable) () "ttk::checkbutton")

(defmethod (setf command) (val (check-button check-button))
  (add-callback (name check-button) val)
  (format-wish "~a configure -command {callbackval ~a $~a}" (widget-path check-button)
               (name check-button) (name check-button))
  val)

(defmethod value ((v check-button))
  (format-wish "global ~a; senddata $~a" (name v) (name v))
  (if (equal 1 (read-data))
      t
      nil))

(defmethod (setf value) (val (v check-button))
  (when (or (equal val 1)
            (equal val 0))
    (warn "Use of 1 and 0 for check-button values is deprecated, use T or NIL. Treating ~A as t"
          val))
  (format-wish "global ~a; set ~a {~a}" (name v) (name v) (if val 1 0))
  val)
