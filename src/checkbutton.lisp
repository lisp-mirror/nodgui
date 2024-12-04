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

(defwrapper check-button (widget tkvariable) () "ttk::checkbutton")

(defmethod (setf command) (val (check-button check-button))
  (add-callback (name check-button) val)
  (format-wish "~a configure -command {callbackval ~a $~a}" (widget-path check-button)
               (name check-button) (name check-button))
  val)

(defmethod value ((v check-button))
  (with-read-data (nil)
    (format-wish "global ~a; senddata $~a" (name v) (name v))
    (if (equal 1 (read-data))
        t
        nil)))

(defmethod (setf value) (val (v check-button))
    (when (and (numberp val)
               (= val 0))
      (warn (strcat "Use of 0 for check-button values will be treated as true,"
                    " so the checkbutton state will be \"on\"")
            val))
    (format-wish "global ~a; set ~a {~a}" (name v) (name v) (if val 1 0))
    val)
