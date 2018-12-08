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

(defargs button (widget)
  command
  compound
  default
  image
  state
  textvariable
  underline
  width)

(defwrapper button (tktextvariable widget) () "ttk::button")

(defmethod (setf command) (val (button button))
  (add-callback (name button) val)
  (format-wish "~a configure -command {callback ~a}" (widget-path button) (name button))
  val)

(defmethod (setf image) ((val photo-image) (button button))
  (format-wish (format nil
                       (tcl-str (~a configure -image ~a))
                       (widget-path button)
                       (name val)))
  button)
