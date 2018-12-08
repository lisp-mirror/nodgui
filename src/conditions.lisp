;; This software is Copyright © 2018 cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :nodgui.conditions)

(define-condition out-of-bounds (error)
  ((seq
    :initarg :seq
    :reader seq)
   (idx
    :initarg :idx
    :reader idx))
   (:documentation "Error when you go out of bound"))

(define-condition nodgui-error (simple-error) ())

(defun nodgui-error (format &rest args)
  (error 'nodgui-error :format-control format :format-arguments args))

(define-condition tk-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (error stream)
             (format stream "Tcl/Tk error: ~a" (message error)))))

(define-condition tk-communication-error (tk-error) ())

(define-constant +event-parsing-error-template+ "Error parsing event ~s. ~a" :test #'string=)

(define-condition event-parsing-error (error)
  ((message
    :initarg :message
    :reader  message)
   (pattern
    :initarg :pattern
    :reader  pattern))
  (:report (lambda (error stream)
             (format stream
                     +event-parsing-error-template+
                     (pattern error)
                     (message error)))))

(define-condition nodgui-event-too-many-fields          (event-parsing-error) ())

(define-condition nodgui-event-field-has-space          (event-parsing-error) ())

(define-condition nodgui-event-invalid-modifier         (event-parsing-error) ())

(define-condition nodgui-event-invalid-detail           (event-parsing-error) ())

(define-condition nodgui-event-duplicate-modifier       (event-parsing-error) ())

(define-condition nodgui-event-incompatible-type-detail (event-parsing-error) ())

(define-condition nodgui-event-invalid-field            (event-parsing-error) ())

(defmacro with-default-on-error ((default) &body body)
  `(handler-case
      (progn ,@body)
    (error () ,default)))
