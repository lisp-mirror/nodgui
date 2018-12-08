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

(defparameter *debug-settings-table*
  (copy-tree
   '(((0 :minimum) . nil)
     ((1 :deploy)  . production-condition-handler)
     ((2 :develop) . graphical-condition-handler)
     ((3 :maximum) . paranoid-condition-handler))))

(defun debug-setting-condition-handler (debug-setting)
  "Given a debug setting (see WITH-NODGUI for details), return the debugger class to use."
  (let* ((debug (if (numberp debug-setting)
                    (min 3 (max 0 (ceiling debug-setting)))
                    debug-setting))
         (cons (assoc (list debug) *debug-settings-table* :test #'intersection)))
    (cond
      ((or (typep debug 'class)
           (and (symbolp debug-setting)
                (ignore-errors (find-class debug))))
       debug)
      (cons (cdr cons))
      (t (error "Unknown debug setting ~S" debug)))))

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
          (handler-bind ((condition generic-handler)
                         (warning warning-handler)
                         (error error-handler))
            (funcall thunk))))
      #'funcall))
