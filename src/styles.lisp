;; This software is Copyright © 2022 cage

;; cage  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui)

(named-readtables:in-readtable nodgui.syntax:nodgui-syntax)

(defun serialize->layout (list)
  (with-output-to-string (stream)
    (labels ((%atom->layout (atom)
               (cond
                 ((numberp atom)
                  (format nil "~a" atom))
                 ((member atom
                          '("children" "expand" "side" "sticky" "border")
                          :test #'string-equal)
                  (format nil "-~(~a~)" atom))
                 (t
                  (format nil "~a" (sanitize atom)))))
             (%list->layout ()
               (cond
                 ((null list)
                  t)
                 ((atom (car list))
                  (format stream "~a " (%atom->layout (car list)))
                  (setf list (rest list))
                  (%list->layout))
                 (t
                  (format stream "-children { ")
                  (setf list (car list))
                  (%list->layout)
                  (format stream "}")))))
      (%list->layout))))

(defun keywordize (a)
  (a:make-keyword (string-upcase a)))

(p:defrule option (or "-expand" "-side" "-sticky" "-border")
  (:function (lambda (a) (keywordize (subseq a 1)))))

(p:defrule children-option "-children"
  (:constant nil))

(p:defrule children (and children-option blanks "{")
  (:constant nil))

(p:defrule blank (or #\space #\Newline #\Tab)
  (:constant nil))

(p:defrule blanks (* blank)
  (:constant nil))

(p:defrule name (p:+ (not (or blank "{" "}" option children-option)))
  (:text t))

(p:defrule options (and option blanks name blanks)
  (:function (lambda (a) (remove-if #'null a))))

(p:defrule elements (p:+ (and name
                              blanks
                              (* options)
                              blanks))
  (:function (lambda (a) (remove-if #'null (a:flatten a)))))

(p:defrule layout (and elements
                       (p:? (and children blanks layout "}"))
                       blanks)
  (:function (lambda (a)
               (let ((elements (elt a 0))
                     (children-block (elt a 1)))
                 (if children-block
                     (append elements (list (elt children-block 2)))
                     elements)))))

(defun insert-layout (source new-node node-name-to-find)
   (cond
     ((null source)
      nil)
     ((consp (first source))
      (append (list (insert-layout (first source) new-node node-name-to-find))
              (insert-layout (rest source) new-node node-name-to-find)))
     ((and (atom (first source))
           (string= (first source)
                    node-name-to-find))
      (append new-node source))
     (t
      (append (list (first source))
              (insert-layout (rest source) new-node node-name-to-find)))))

(defun parse-layout (widget-layout)
  (p:parse 'layout widget-layout :junk-allowed nil))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *styles* '())

  (defclass style ()
    ((name
      :initform nil
      :initarg  :name
      :accessor name)
     (parent
      :initform nil
      :initarg  :parent
      :accessor parent)
     (options
      :initform nil
      :initarg  :options
      :accessor options)
     (applied
      :initform nil
      :initarg  :applied
      :reader   appliedp
      :writer   (setf applied))
     (action
      :initform nil
      :initarg :action
      :reader action)
     (pre-application-function
      :initform (constantly t)
      :initarg :pre-application-function
      :accessor pre-application-function
      :type function)))

  (defmethod initialize-instance :after ((object style)
                                         &key
                                           (push-to-styles t)
                                         &allow-other-keys)
    (when push-to-styles
      (setf *styles* (remove-if (lambda (a) (string= (name a) (name object))) *styles*))
      (push object *styles*))
    object)

  (defmethod print-object ((object style) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "name: ~s options: ~a" (name object) (options object)))))

(defmethod (setf action) ((object style) val)
  (assert (or (eq val :configure)
              (eq val :create)))
  (setf (slot-value object 'action) val))

(defgeneric root-style-p (object))

(defmethod root-style-p ((object style))
  (null (parent object)))

(defmacro create-root-styles (&rest names)
  `(progn
     ,@(loop for name in names
             collect
             (a:with-gensyms (style)
               `(let ((,style (make-instance 'style
                                             :applied nil
                                             :name ,name
                                             :push-to-styles nil)))
                  (a:define-constant
                      ,(nodgui.utils:format-fn-symbol t  "~:@(+~a+~)" name)
                      ,style
                    :test (lambda (a b) (string= (name a) (name b))))
                  (push ,style *styles*))))))

(create-root-styles "TButton"
                    "TCheckbutton"
                    "TCombobox"
                    "TEntry"
                    "TFrame"
                    "TLabel"
                    "TLabelFrame"
                    "TMenubutton"
                    "TNotebook"
                    "TPanedwindow"
                    "Horizontal.TProgressbar"
                    "Vertical.TProgressbar"
                    "TRadiobutton"
                    "Horizontal.TScale"
                    "Vertical.TScale"
                    "Horizontal.TScrollbar"
                    "Vertical.TScrollbar"
                    "TSeparator"
                    "TSizegrip"
                    "Treeview")

(defgeneric apply-style (object))

(defun serialize-style-name (style)
  (labels ((concat-name (style &optional (accum (name style)))
             (if (root-style-p style)
                 accum
                 (let ((parent (find-style (parent style))))
                   (when (null parent)
                     (error (format nil
                                    "style ~a inherith from ~a but the latter was not specified"
                                    (name style)
                                    (parent style))))
                   (concat-name parent (join-with-strings* "."
                                                           (name style)
                                                           (name parent)))))))
    (concat-name style)))

(defmethod apply-style ((object style))
  (with-accessors ((parent                   parent)
                   (options                  options)
                   (name                     name)
                   (action                   action)
                   (pre-application-function pre-application-function)) object
    (assert (or (null action)
                (eq action :element-create)))
    (when (not (appliedp object))
      (let ((actual-name (serialize-style-name object)))
        (funcall pre-application-function)
        (if (eq action :element-create)
            (format-wish "ttk::style element create ~a ~{~(~a~) ~({~a}~) ~}"
                         actual-name options)
            (format-wish "ttk::style configure ~a ~{-~(~a~) ~({~a}~) ~}"
                         actual-name options))))))

(defgeneric symbol->stylename (object))

(defmethod symbol->stylename ((object string))
  (if (string-empty-p object)
      (error "A style name can not be the empty string")
      object))

(defmethod symbol->stylename ((a symbol))
  (symbol-name a))

(defmethod symbol->stylename ((a null))
  (error "A style name can not be null"))

(defmacro make-style (name (&key (extend nil) (action nil)) &rest options-pairs)
  `(make-instance 'style
                  :name    ,(symbol->stylename name)
                  :parent  ,(cond
                              ((null extend)
                               nil)
                              ((symbolp extend)
                               (symbol->stylename extend))
                              (t
                               (to-s extend)))
                  :action  ,action
                  :options (list ,@options-pairs)))

(defgeneric style-configure (object style))

(defmethod style-configure (object (style style))
  (configure object :style (serialize-style-name style)))

(defun find-style (style-name)
  (find-if (lambda (a) (string= (name a) style-name))
           *styles*))

(defmethod style-configure (object (style string))
  (style-configure object (find-style style)))

(defmethod style-configure (object (style symbol))
  (style-configure object (symbol->stylename style)))

(defgeneric fetch-layout (object))

(defmethod fetch-layout ((object string))
  (with-read-data (nil)
    (let ((name (find-style object)))
      (format-wish "senddatastring [ttk::style layout ~a]"
                   (serialize-style-name name))
      (let ((raw (read-data)))
        (parse-layout raw)))))

(defmethod fetch-layout ((object symbol))
  (with-read-data (nil)
    (let ((name (find-style (symbol->stylename object))))
      (format-wish "senddatastring [ttk::style layout ~a]"
                   (serialize-style-name name))
      (let ((raw (read-data)))
        (parse-layout raw)))))

(defmethod fetch-layout ((object style))
  (with-read-data (nil)
    (format-wish "senddatastring [ttk::style layout ~a]"
                 (serialize-style-name object))
    (let ((raw (read-data)))
      (parse-layout raw))))

(defgeneric layout-configure (object layout))

(defmethod layout-configure ((object style) (layout list))
  (layout-configure (name object) layout))

(defmethod layout-configure ((object symbol) layout)
  (layout-configure (symbol->stylename object) layout))

(defmethod layout-configure ((object string) layout)
  (send-wish (format nil
                     "ttk::style layout ~a { ~a }"
                     (serialize-style-name (find-style object))
                     (serialize->layout layout))))
