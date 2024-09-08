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

  (defparameter *suppress-newline-for-tcl-statements*            nil)

  (defparameter *add-space-after-emitted-string*                 nil)

  (defparameter *add-space-after-emitted-unspecialized-element*  t)

  (defparameter *stringify-keyword*                              nil)

  (defparameter *sanitize*                                       nil)

  (defparameter *in-braces*                                      nil)

  (defparameter *do-escape-tilde*                                t)

  (a:define-constant +to-lisp-mode+   :lisp  :test #'eq)

  (a:define-constant +to-tcl-if-mode+ :if    :test #'eq)

  (a:define-constant +to-tcl-group+   :group :test #'eq)

  (defstruct tcl/code (data))

  (defstruct (tcl/list (:include tcl/code)))

  (defstruct (tcl/keyword (:include tcl/code))
    (raw-keyword  :none)
    (spaces-after "")
    (as-string-p  nil))

  (defstruct (tcl/< (:include tcl/code)))

  (defstruct (tcl/> (:include tcl/code)))

  (defmacro with-no-emitted-newline (&body body)
    `(let ((*suppress-newline-for-tcl-statements* t))
       ,@body))

  (defmacro with-no-emitted-spaces (&body body)
    `(let ((nodgui.tcl-emitter:*add-space-after-emitted-string* nil)
           (nodgui.tcl-emitter:*add-space-after-emitted-unspecialized-element* nil))
       ,@body))

  (defmacro with-emitted-spaces (&body body)
    `(let ((nodgui.tcl-emitter:*add-space-after-emitted-string* t)
           (nodgui.tcl-emitter:*add-space-after-emitted-unspecialized-element* t))
       ,@body))

  (defmacro with-stringify-keyword (&body body)
    `(let ((*stringify-keyword* t))
       ,@body))

  (defmacro with-no-escape-tilde (&body body)
    `(let ((*do-escape-tilde* nil))
       ,@body))

  (defmacro with-recursive-tclize (&body body)
    (lambda () (tclize `(,@body))))

  (defun tag (element)
    (cond
      ((eq '< element)
       (make-tcl/<))
      ((eq '> element)
       (make-tcl/>))
      (t
       element)))

  (defgeneric ->tcl (code))

  (defmethod ->tcl ((code function))
    code)

  (defmethod ->tcl ((code bypass-escape))
    (let ((*sanitize* nil))
      (->tcl (bypass-escape-data code))))

  (defun %sanitize (object)
    (if *sanitize*
        (if *in-braces*
            (sanitize (make-escape-only-braces :data object))
            (sanitize object))
        object))

  (defmethod ->tcl (code)
    (lambda ()
      (if (and (not *in-braces*)
               *add-space-after-emitted-unspecialized-element*)
          (format nil "~a " (%sanitize code))
          (format nil "~a"  (%sanitize code)))))

  (defmethod ->tcl ((code string))
    (lambda ()
      (if (and (not *in-braces*)
               *add-space-after-emitted-string*)
          (format nil "~a " (%sanitize code))
          (%sanitize code))))

  (defmethod ->tcl ((code tcl/<))
    (lambda () "\"(\" "))

  (defmethod ->tcl ((code tcl/>))
    (lambda () " \")\""))

  (defmethod ->tcl ((code symbol))
    (flet ((calc-spaces (key)
             (multiple-value-bind (whole plus-signs)
                 (cl-ppcre:scan-to-strings "(\\++)%?$" key)
               (if whole
                   (1- (length (a:first-elt plus-signs)))
                   -1))))
      (lambda ()
        (let* ((key           (symbol-name code))
               (as-string-p   (scan "%$" key))
               (escapep       (string= "." (a:first-elt key)))
               (spaces-after  (if (< (calc-spaces key) 0)
                                  " "
                                  (make-string (calc-spaces key) :initial-element #\Space)))
               (keywordp      (keywordp code))
               (removed-+-res (regex-replace "\\++%?$" key ""))
               (res-raw       (format nil "~:[~;:~]~(~a~)" keywordp removed-+-res))
               (rest-chars    (subseq key 1))
               (first-char    (a:first-elt key)))
          (declare (dynamic-extent key first-char rest-chars))
          (when (and (not as-string-p)
                     (char= first-char #\{))
            (setf *in-braces* t))
          (when (and (not as-string-p)
                     (char= first-char #\}))
            (setf *in-braces* nil))
          (cond
            (escapep
             (if as-string-p
                 (format nil "\"~a\"" (regex-replace "%$" rest-chars ""))
                 (format nil "~a"     rest-chars)))
            (keywordp
             (->tcl (make-tcl/keyword :data         res-raw
                                      :raw-keyword  code
                                      :spaces-after spaces-after
                                      :as-string-p  as-string-p)))
            (t
             (if as-string-p
                 (format nil "\"~a\"~a" (regex-replace "%$" res-raw "") spaces-after)
                 (format nil "~a~a" res-raw spaces-after))))))))

  (defmethod ->tcl ((code tcl/keyword))
    (if *stringify-keyword*
        (to-s (tcl/keyword-raw-keyword code))
        (let ((no-colon (subseq (tcl/keyword-data code) 1))
              (spaces   (tcl/keyword-spaces-after code)))
          (if (tcl/keyword-as-string-p code)
              (format nil "\"~a\"~a"
                      (regex-replace "%$" no-colon "")
                      spaces)
              (format nil "~a~a" (tcl/keyword-data code) spaces)))))

  (defmacro %evl (body)
    (a:with-gensyms (res)
      `(let ((,res (a:lastcar (loop for i in (rest ,body) collect (eval i)))))
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
       (list (lambda () (stringify-all (a:flatten (->tcl (rest code)))))))
      ((eq (first code) +to-tcl-if-mode+)
       ;; (:if test pass fail)
       (list (lambda ()
               (stringify-all
                (a:flatten (list (->tcl 'if)
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

  (defmacro for-list (var l &body body)
    `(loop for ,var in ,l collect
          (progn ,@body)))

  (defun %to-safe-format-string (a)
    a)

  (defun force-string-read-macro (stream char ign)
    (declare (ignore char ign))
    (warn (strcat "[NOTICE] The #[ ... ] reader macro is useless "
                  "and will be removed in a future releases of "
                  "the library.~%"))
    (let ((raw (read-delimited-list #\] stream)))
      (if (= (length raw) 1)
          `(%to-safe-format-string ,(first raw))
          `(%to-safe-format-string ,raw))))

  (named-readtables:defreadtable nodgui-force-escape-syntax
    (:fuse :standard)
    (:dispatch-macro-char #\# #\[ #' force-string-read-macro))

  (defmacro tclize (statement &key (sanitize t))
    (a:with-gensyms (unescaped)
    `(let* ((*sanitize* ,sanitize)
            (,unescaped  (stringify-all (a:flatten (->tcl ,statement)))))
       (if *do-escape-tilde*
           (escape-~ ,unescaped)
           ,unescaped))))

  (defmacro empty-string-if-nil (value statement)
    `(if ,value
         ,statement
         ""))

  (defmacro defproc (name args &body body)
    `(symbol-macrolet ((exp (format nil
                                    "~%proc ~a { ~a } {~%~{    ~a~}}~%"
                                    ,(funcall (->tcl name))
                                    ,(let ((*suppress-newline-for-tcl-statements* t))
                                       (tclize args))
                                    (tcl ,@body))))
       exp))

  (defun keyword->tcl (keyword &key (downcase nil))
    (with-no-emitted-newline
      (with-stringify-keyword
        (let ((res (tclize `(,keyword))))
          (if downcase
              (string-downcase res)
              res))))))
