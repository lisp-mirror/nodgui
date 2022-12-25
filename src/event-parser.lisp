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

;; EVENT         := PATTERN | PATTERN FILLER PATTERN
;; PATTERN       := '<' (MODIFIER '-')* (TYPE | MODIFIER '-')? DETAIL '>' |
;;                  '<' (MODIFIER '-')* TYPE  '>'                         |
;;                  '<<' VIRTUAL-EVENT '>>'
;; MODIFIER      := one of the modifier in event-symbol.lisp
;; TYPE          := one of type in event-symbol.lisp
;; DETAIL        := [A-z,a-z,0-9]+ or one of the detail-value in event-symbol.lisp
;; VIRTUAL-EVENT := a string of alphanumeric charaters except < or >
;; FILLER        := BLANK*
;; BLANK         := #\space | #\Tab

(define-constant +field-separator+ "-" :test #'string=)

(define-constant +left-delimiter+  "<" :test #'string=)

(define-constant +right-delimiter+ ">" :test #'string=)

(defparameter    *check-more-parsing-errors*   t)

(p:defrule virtual-event (p:+ (not (or #\< #\>)))
  (:text t))

(p:defrule separator #\-
  (:text t))

(p:defrule modifier (p:+ (not (or #\< #\> separator #\Space)))
  (:text t))

(p:defrule event-type modifier)

(p:defrule detail (p:+ (not #\>))
  (:text t))

(p:defrule virtual-event (and #\< #\< (p:+ (not #\>)) #\> #\>)
  (:function (lambda (a) (strcat "<" (coerce (third a) 'string) ">"))))

(defun remove-separators (field)
  (remove-if (lambda (a) (member a '("<" ">" "-") :test #'string=))
             (a:flatten field)))

(p:defrule complex-pattern (and #\<
                                (p:* (and modifier separator))
                                (p:? (and (or event-type modifier) separator))
                                detail
                                #\>)
  (:function (lambda (a) (check-pattern (remove-separators a)))))

(p:defrule pattern (or virtual-event
                       complex-pattern))

(p:defrule blank (or #\space #\Tab)
  (:constant nil))

(p:defrule filler (* blank)
  (:constant nil))

(p:defrule event   (and pattern (p:? (and filler event)))
  (:function (lambda (a)
               (let* ((pattern    (first a))
                      (other-term (second (second a))))
                 (format nil
                         "<~a>~@[~a~]"
                         (pattern->string pattern)
                         other-term)))))

(defun check-pattern (pattern)
  (when *check-more-parsing-errors*
    (check-spaces       pattern)
    (check-valid-detail-or-type pattern)
    (when (> (length pattern) 1)
      (check-valid-type-or-modifier pattern)
      (check-compatibility-type-detail pattern))
    (when (> (length pattern) 2)
      (check-valid-modifiers pattern)
      (check-different-modifiers pattern)))
  pattern)

(defun pattern->string (pattern)
  (nodgui.utils:join-with-strings pattern +field-separator+))

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

(defun parse-event (event-string)
  (p:parse 'event event-string))

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
