;; Copyright (c) 2013 Masataro Asai (guicho2.71828@gmail.com)
;; Licensed under the LLGPL License.

(in-package :nodgui.typed-operations)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-typed-op (name op type
                             &key
                               (return-value-on t)
                               (input-value-on t))
    `(defmacro ,name (&rest args)
       (let ((the-args
              ,(if input-value-on
                   `(mapcar #'(lambda (argsym)
                                `(the ,',type ,argsym))
                            args)
                   `args)))
         ,(if return-value-on
              ``(the ,',type
                     (,',op ,@the-args))
              ``(,',op ,@the-args)))))

  (defmacro class-of-desired-type () `(find-class 'single-float))

  (deftype desired-type () 'single-float)

  (declaim (inline desired))

  (defun desired (arg)
    (coerce arg 'desired-type))

  (declaim (inline d))

  (defun d (a)
    (desired a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ops

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-typed-op d+ + desired-type)

  (define-typed-op d* * desired-type)

  (define-typed-op d- - desired-type)

  (define-typed-op d/ / desired-type)

  (define-typed-op dsqrt sqrt desired-type)

  (define-typed-op daref aref desired-type :input-value-on nil)

  (define-typed-op dmin min desired-type)

  (define-typed-op dmax max desired-type)

  (define-typed-op d> > desired-type :return-value-on nil)

  (define-typed-op d< < desired-type :return-value-on nil)

  (define-typed-op d>= >= desired-type :return-value-on nil)

  (define-typed-op d<= <= desired-type :return-value-on nil)

  (define-typed-op d= = desired-type :return-value-on nil)

  (define-typed-op drandom random desired-type)

  (define-typed-op dslot-value slot-value desired-type :input-value-on nil)

  (define-typed-op dsetf setf desired-type :input-value-on nil)

  (define-typed-op dabs abs desired-type :return-value-on nil)

  (define-typed-op dplusp plusp desired-type :return-value-on nil)

  (define-typed-op dminusp minusp desired-type :return-value-on nil)

  (define-typed-op dzerop zerop desired-type :return-value-on nil)

  (define-typed-op dcos cos desired-type)

  (define-typed-op dsin sin desired-type)

  (define-typed-op dtan tan desired-type)

  (define-typed-op dcosh cosh desired-type)

  (define-typed-op dsinh sinh desired-type)

  (define-typed-op dtanh tanh desired-type)

  (define-typed-op dacos acos desired-type)

  (define-typed-op dasin asin desired-type)

  (define-typed-op datan atan desired-type)

  (define-typed-op dexp exp desired-type)

  (define-typed-op dexpt expt desired-type)

  (define-typed-op dlog log desired-type)

  (define-typed-op dfloor floor desired-type))

(defun dlerp (w a b)
  #.nodgui.config:default-optimization
  (declare (type desired-type w a b))
  (d+ a (d* w (d- b a))))

(defun secure-dacos (a)
  #.nodgui.config:default-optimization
  (declare (desired-type a))
  (dacos (dmin 1f0 a)))

(alexandria:define-constant +degree->radians-factor+ (d (/ pi 180)) :test #'=)

(declaim (inline degree->radians))

(defun degree->radians (degree)
  #.nodgui.config:default-optimization
  (declare (desired-type degree))
  (d* degree +degree->radians-factor+))

(alexandria:define-constant +radians->degree-factor+ (d (/ 180 pi)) :test #'=)

(declaim (inline radians->degree))

(defun radians->degree (radians)
  #.nodgui.config:default-optimization
  (declare (desired-type radians))
  (d* radians +radians->degree-factor+))

;;;; constants

(a:define-constant +2pi+ (d (* 2.0f0 pi)) :test #'=)

(a:define-constant +pi/2+ (d (* 1/2 pi)) :test #'=)

;; parsing helper

(defun parse-number->desired (num)
  (desired (parse-number:parse-number num)))

(declaim (inline f))

(defun f (num)
  (coerce num 'fixnum))

(defmacro f* (&body body)
  `(the fixnum (* ,@body)))

(defmacro f/ (&body body)
  `(the fixnum (/ ,@body)))

(defmacro f+ (&body body)
  `(the fixnum (+ ,@body)))

(defmacro f- (&body body)
  `(the fixnum (- ,@body)))

(defmacro faref (&body body)
  `(the fixnum (aref ,@body)))

(defmacro f< (&body body)
  `(the boolean (< ,@body)))

(defmacro f> (&body body)
  `(the boolean (> ,@body)))

(defmacro f<= (&body body)
  `(the boolean (<= ,@body)))

(defmacro f>= (&body body)
  `(the boolean (>= ,@body)))

(defmacro frem (&body body)
  `(the fixnum (rem ,@body)))

(defmacro fabs (&body body)
  `(the fixnum (abs ,@body)))
