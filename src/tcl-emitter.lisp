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

(in-package :nodgui.tcl-emitter)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *suppress-newline-for-tcl-statements* nil)

  (defparameter *add-space-after-emitted-string*      nil)

  (defparameter *add-space-after-emitted-unspecialized-element*  t)

  (defparameter *sanitize*                                       nil)

  (define-constant +to-lisp-mode+   :lisp  :test #'eq)

  (define-constant +to-tcl-if-mode+ :if    :test #'eq)

  (define-constant +to-tcl-group+   :group :test #'eq)

  (defstruct tcl/code (data))

  (defstruct (tcl/list (:include tcl/code)))

  (defstruct (tcl/keyword (:include tcl/code)))

  (defstruct (tcl/< (:include tcl/code)))

  (defstruct (tcl/> (:include tcl/code)))

  (defun tag (element)
    (cond
      ((eq '< element)
       (make-tcl/<))
      ((eq '> element)
       (make-tcl/>))
      (t
       element)))

  (defgeneric ->tcl (code))

  (defmethod ->tcl (code)
    (let ((actual-code (if *sanitize*
                           (sanitize code)
                           code)))
      (if *add-space-after-emitted-unspecialized-element*
          (lambda () (format nil "~a " actual-code))
          (lambda () (format nil "~a"  actual-code)))))

  (defmethod ->tcl ((code string))
    (let ((actual-code (if *sanitize*
                           (sanitize code)
                           code)))
      (if  *add-space-after-emitted-string*
           (lambda () (format nil "~a " actual-code))
           (lambda () (format nil "~a"  actual-code)))))

  (defmethod ->tcl ((code tcl/<))
    (lambda () "\"(\" "))

  (defmethod ->tcl ((code tcl/>))
    (lambda () " \")\""))

  (defmethod ->tcl ((code symbol))
    (flet ((calc-spaces (key)
             (multiple-value-bind (whole plus-signs)
                 (cl-ppcre:scan-to-strings "(\\++)%?$" key)
               (if whole
                   (1- (length (first-elt plus-signs)))
                   -1))))
      (lambda ()
        (let* ((key           (symbol-name code))
               (as-string-p   (scan "%$" key))
               (escapep       (string= "." (first-elt key)))
               (spaces-after  (if (< (calc-spaces key) 0)
                                  " "
                                  (make-string (calc-spaces key) :initial-element #\Space)))
               (keywordp      (keywordp code))
               (removed-+-res (regex-replace "\\++%?$" key ""))
               (res-raw       (format nil "~:[~;:~]~(~a~)" keywordp removed-+-res)))
          (cond
            (escapep
             (if as-string-p
                 (format nil "\"~a\"" (regex-replace "%$" (subseq key 1) ""))
                 (format nil "~a"     (subseq key 1))))
            (t
             (if (or keywordp
                     as-string-p)
                 (if as-string-p
                     (format nil "\"~a\"~a" (regex-replace "%$" res-raw "") spaces-after)
                     (format nil "\"~a\"~a" res-raw spaces-after))
                 (format nil "~a~a" res-raw spaces-after))))))))

  (defmacro %evl (body)
    (with-gensyms (res)
      `(let ((,res (lastcar (loop for i in (rest ,body) collect (eval i)))))
         (strcat* (mapcar #'funcall
                          (->tcl (if (listp ,res) ,res (list ,res))))))))

  (defun funcall-all (v)
    (mapcar #'funcall v))

  (defun stringify-all (v)
    ;; this replace  is a  shameless plug to  remove the
    ;; space at the end of the last token :(
    (regex-replace "\\s+\\n$"
                   (strcat* (mapcar #'funcall v))
                   (string #\Newline)))

  (defmethod ->tcl ((code list))
    (cond
      ((eq (first code) +to-lisp-mode+)
       (list (lambda () (%evl code))))
      ((eq (first code)  +to-tcl-group+)
       (list (lambda () (stringify-all (flatten (->tcl (rest code)))))))
      ((eq (first code) +to-tcl-if-mode+)
       ;; (:if test pass fail)
       (list (lambda ()
               (stringify-all
                (flatten (list (->tcl 'if)
                               (->tcl `({ ,(stringify-all (->tcl (second code))) } {))
                               (->tcl `(,(stringify-all (->tcl (third code)))))
                               (->tcl '(} else {))
                               (->tcl `(,(stringify-all (->tcl (fourth code))) }))))))))
      (t
       (->tcl (make-tcl/list :data (append code (and (not *suppress-newline-for-tcl-statements*)
                                                     (list (string #\Newline)))))))))

  (defmethod ->tcl ((code (eql nil)))
    (list (lambda () "")))

  (defmethod ->tcl ((code tcl/list))
    (loop for i in (tcl/list-data code) collect
         (->tcl (tag i))))

  (defmacro tcl (&body body)
    `(list ,@(loop for statement in body collect
                ;; this replace  is a  shameless plug to  remove the
                ;; space at the end of the last token :(
                  (let ((res (->tcl statement)))
                    (stringify-all res)))))

  (defmacro tcl-str (&body body)
    (strcat* (loop for statement in body collect
                ;; this replace  is a  shameless plug to  remove the
                ;; space at the end of the last token :(
                  (let ((res (->tcl statement)))
                    (stringify-all res)))))

  (defmacro with-flush-server (body)
    (append body '((flush $server))))

  (defmacro defproc (name args &body body)
    `(symbol-macrolet ((exp (format nil
                                    "~%proc ~a{~{~a~}} {~%~{    ~a~}}~%"
                                    ,(funcall (->tcl name))
                                    ',(mapcar #'funcall (->tcl args))
                                    (tcl ,@body))))
       exp))

  (defmacro for-list (var l &body body)
    `(loop for ,var in ,l collect
          (progn ,@body)))


  (defun force-string-read-macro (stream char ign)
    (declare (ignore char ign))
    (let ((raw (read-delimited-list #\] stream)))
      (if (= (length raw) 1)
          `(to-s ,(first raw))
          `(to-s ,raw))))

  (cl-syntax:defsyntax nodgui-force-escape-syntax
    (:merge :standard)
    (:macro-char #\# :dispatch)
    (:dispatch-macro-char #\# #\[ #' force-string-read-macro))

  (cl-syntax:use-syntax nodgui-force-escape-syntax)

  (defmacro tclize (statement &key (sanitize t))
    `(let ((*sanitize* ,sanitize))
       (stringify-all (flatten (->tcl ,statement)))))

  (defmacro empty-string-if-nil (value statement)
    `(if ,value
         ,statement
         "")))
