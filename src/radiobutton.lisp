;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui)

(defargs radio-button ()
  command-radio-button
  class
  compound
  cursor
  image
  state
  style
  takefocus
  textvariable
  underline
  value-radio-button
  variable-radio-button
  width)

(defwrapper radio-button (tktextvariable widget)
  ((val
    :accessor radio-button-value
    :initarg  :value
    :initform nil)
   (var
    :accessor radio-button-variable
    :initarg :variable
    :initform nil))
  "ttk::radiobutton")

(defmethod value ((rb radio-button))
  "reads the content of the shared variable of the radio button set"
  (if (radio-button-variable rb)
      (progn
        (format-wish "global ~a; senddata ${~a}"
                     (radio-button-variable rb)
                     (radio-button-variable rb))
        (read-data))
      nil))

(defmethod (setf value) (val (rb radio-button))
  "sets the content of the shared variable of the radio button set"
  (when (radio-button-variable rb)
    (format-wish "global ~a; set ~a {~a}"
                 (radio-button-variable rb)
                 (radio-button-variable rb) val))
  val)

(defmethod (setf command) (val (rb radio-button))
  (add-callback (name rb) val)
  (format-wish "~a configure -command {global ~a;callbackval ~a $~a}"
               (widget-path rb)
               (name rb)
               (name rb)
               (radio-button-variable rb))
  val)
