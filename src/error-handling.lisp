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

(defun make-call-with-condition-handlers-function (handler-class)
  "Return a function that will call a thunk with the appropriate condition handlers in place."
  (if handler-class
      (let ((warning-handler (make-condition-handler-function
                              :class handler-class :title "Warning"))
            (error-handler (make-condition-handler-function
                            :class handler-class :title "Error"))
            (generic-handler (make-condition-handler-function
                              :class handler-class :title "Attention")))
        (lambda (thunk)
          (handler-bind ((error error-handler)
                         (warning warning-handler)
                         (condition generic-handler))
            (funcall thunk))))
      #'funcall))
