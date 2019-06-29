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

(in-package :nodgui.event-parser)

;; EVENT         := PATTERN | PATTERN FILLER* PATTERN

;; PATTERN       := '<' (MODIFIER '-')* (TYPE | MODIFIER '-')? DETAIL '>' |
;;                  '<' (MODIFIER '-')* TYPE  '>'                         |
;;                  '<<' VIRTUAL-EVENT '>>'

;; MODIFIER      := one of the modifier in event-symbol.lisp

;; TYPE          := one of type in event-symbol.lisp

;; DETAIL        := [A-z,a-z,0-9]+ or one of the detail-value in event-symbol.lisp

;; VIRTUAL-EVENT := a string of alphanumeric charaters except < or >

(define-constant +max-num-fields+    4 :test #'=)

(define-constant +field-separator+ "-" :test #'string=)

(define-constant +left-delimiter+  "<" :test #'string=)

(define-constant +right-delimiter+ ">" :test #'string=)

(defparameter    *check-more-parsing-errors*   t)

(defmacro gen-lexer-constant (name)
  `(define-constant
       ,(nodgui.utils:format-fn-symbol t "+~a+" name)
       ,(make-keyword name)
     :test #'eq))

(gen-lexer-constant delim-left)

(gen-lexer-constant delim-right)

(gen-lexer-constant delim-field)

(gen-lexer-constant field)

(gen-lexer-constant filler)

(defmacro gen-lexer ()
  `(define-string-lexer lexer
     (,+left-delimiter+
      (return (values +delim-left+  +delim-left+)))
     (,+right-delimiter+
      (return (values +delim-right+ +delim-right+)))
     (,+field-separator+
      (return (values +delim-field+  +delim-field+)))
     ("\\s+"
      (return (values +filler+ +filler+)))
     ("[\\w\\s]+"
      (return (values +field+ $@)))))

(defmacro gen-parser ()
  `(yacc:define-parser *parser*
     (:print-derives-epsilon t)
     (:start-symbol event)
     (:terminals ,(list +delim-left+ +delim-right+ +delim-field+ +field+ +filler+))
     (event
      (delim-left delim-left field delim-right delim-right #'build-virtual-event)
      (patterns                                            #'identity))
     (patterns
      ()
      (filler patterns                                     #'ignore-filler)
      (delim-left fields delim-right patterns              #'build-pattern))
     (fields
      ()
      (field delim-field fields                            #'append-fields)
      (field                                               #'identity))
     (delim-left
      ,+delim-left+)
     (delim-right
      ,+delim-right+)
     (delim-field
      ,+delim-field+)
     (filler
      ,+filler+)
     (field
      (,+field+))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun pattern->string (pattern)
    (nodgui.utils:join-with-strings pattern +field-separator+))

  (defun ignore-filler (a b)
    (declare (ignore a))
    b)

  (defun append-fields (a b c)
    (declare (ignore b))
    (append a c))

  (defun check-spaces (pattern)
    (loop for field in pattern do
         (when (scan "\\s" field)
           (error 'nodgui-event-field-has-space
             :pattern (pattern->string pattern)
             :message (format nil
                              "Spaces not allowed in field ~s"
                              field)))))

  (defun pattern-detail (pattern)
    (last-elt pattern))

  (defun %find (field bag)
    (find field bag :test #'string=))

  (defun pattern-type (pattern)
    (elt pattern (- (length pattern) 2)))

  (defun check-valid-type (pattern)
    (let ((type (pattern-type pattern)))
      (when (not (%find type *all-event-type*))
           (error 'nodgui-event-invalid-type
             :pattern (pattern->string pattern)
             :message (format nil
                              "Invalid type ~s"
                              type)))))

  (defun check-valid-detail-or-type (pattern)
    (let ((detail (pattern-detail pattern)))
      (when (and (not (%find detail *all-event-type*))
                 (not (%find detail *all-event-details*))
                 (not (scan "^[a-zA-Z0-9]$" detail)))
           (error 'nodgui-event-invalid-detail
             :pattern (pattern->string pattern)
             :message (format nil
                              "Invalid detail ~s"
                              detail)))))

  (defun check-compatibility-type-detail (pattern)
    (let ((type   (pattern-type   pattern))
          (detail (pattern-detail pattern)))
      (cond
        ((and (find type (list +buttonpress+
                               +button+
                               +buttonrelease+)
                    :test #'string=)
              (not (scan "^[0-5]$" detail)))
         (error 'nodgui-event-incompatible-type-detail
                :pattern (pattern->string pattern)
                :message (format nil
                                 "Detail ~s wants a number in the range [0-5]."
                                 type))))))

  (defun check-valid-modifiers (pattern)
    (let ((modifiers (subseq pattern 0 (- (length pattern) 2))))
      (loop for modifier in modifiers do
           (when (not (%find modifier *all-event-modifier*))
             (error 'nodgui-event-invalid-modifier
                    :pattern (pattern->string pattern)
                    :message (format nil
                                     "Invalid modifier ~s"
                                     modifier))))))

  (defun check-different-modifiers (pattern)
    (let ((modifiers (subseq pattern 0 (1- (length pattern)))))
      (loop for i from 0 below (length modifiers) do
           (when (find (elt modifiers i) (delete@ modifiers i) :test #'string=)
             (error 'nodgui-event-duplicate-modifier
                    :pattern (pattern->string pattern)
                    :message (format nil
                                     "Duplicate modifier ~s"
                                     (elt modifiers i)))))))

  (defun check-valid-type-or-modifier (pattern)
    (let ((type (pattern-type pattern)))
      (when (and (not (%find type *all-event-type*))
                 (not (%find type *all-event-modifier*)))
           (error 'nodgui-event-invalid-field
             :pattern (pattern->string pattern)
             :message (format nil
                              "Invalid field ~s, only type or modifier are allowed here"
                              type)))))


  (defun build-virtual-event (a b c d e)
    (declare (ignore a b d e))
    (strcat +left-delimiter+ +left-delimiter+
            (elt c 0)
            +right-delimiter+ +right-delimiter+))

  (defun build-pattern (a pattern c other-patterns)
    (declare (ignore a c))
    (when *check-more-parsing-errors*
      (check-spaces       pattern)
      (check-valid-detail-or-type pattern)
      (when (> (length pattern) 1)
        (check-valid-type-or-modifier pattern)
        (check-compatibility-type-detail pattern))
      (when (> (length pattern) 2)
        (check-valid-modifiers pattern)
        (check-different-modifiers pattern)))
    (append (list pattern) other-patterns))

  (gen-lexer)

  (gen-parser))

(defun parse-event (event-string)
  (let ((raw (parse-with-lexer (lexer event-string) *parser*)))
    (if (stringp raw)
        raw
        (let ((res      "")
              (patterns (loop for pattern in raw collect
                             (join-with-strings  pattern +field-separator+))))
          (loop for pattern in patterns collect
               (setf res (strcat res +left-delimiter+ pattern +right-delimiter+)))
          res))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun read-event-macro (stream char ign)
    (declare (ignore char ign))
    (let ((raw (do* ((r   (read-char stream) (read-char stream))
                     (res '()))
                    ((char= r #\$) (coerce (reverse res) 'string))
                 (push r res))))
      (parse-event raw)))

  (named-readtables:defreadtable nodgui-event-syntax
    (:fuse :standard)
    (:dispatch-macro-char #\# #\$ #'read-event-macro)))
