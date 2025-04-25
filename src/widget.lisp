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

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(defclass tkobject ()
  ((name
    :accessor name
    :initarg  :name
    :initform nil))
  (:documentation "Base class for every Tk object"))

(defun pprint-down (stream object colon at)
  "Print OBJECT to  STREAM, downcasing unless OBJECT is  a string, and
giving the path of tkobjects."
  (declare (ignore colon at))
  (write-string (down object) stream))

(defgeneric down (object))

(defmethod down (object)
  (format nil "~(~a~)" object))

(defmethod down ((object string))
  object)

(defmethod down ((object tkobject))
  (name object))

(defclass widget (tkobject)
  ((master
    :accessor master
    :initarg :master
    :initform nil
    :documentation "parent widget or nil")
   (widget-path
    :initarg  :path
    :initform nil
    :accessor %widget-path
    :documentation "pathname to refer to the widget")
   (init-command
    :accessor init-command
    :initform nil
    :initarg  :init-command))
  (:documentation "Base class for all widget types"))

(defmethod down ((object widget))
  (widget-path object))

(defmethod initialize-instance :after ((w widget) &key)
  (unless (name w) ; generate name if not given
    (setf (name w) (create-name))))

(defgeneric wrap-length (object widget))

(defmethod wrap-length ((object widget) (length-specification number))
  (configure object :wraplength (truncate (max length-specification 0))))

(defmethod wrap-length ((object widget) (length-specification string))
  (assert (cl-ppcre:scan "[0-9]+[cimp]?" length-specification))
  (configure object :wraplength length-specification))


;; around - initializer

(defmethod initialize-instance :around ((w widget) &key pack place grid)
  (call-next-method)
  ;; pack widget if parameter has been supplied
  (when pack
    (apply #'pack w pack))
  (when place
    (apply #'place w place))
  (when grid
    (apply #'grid w grid)))

(defvar *tk* (make-instance 'widget :name "." :path ".")
  "Dummy widget to access the tk root object")

(defun root-toplevel ()
  *tk*)

(defun root-toplevel-title ()
  (cget (root-toplevel) :title))

(defun set-root-toplevel-title (value)
  (wm-title (root-toplevel) value))

;;; busy functions

(defgeneric busy-hold (toplevel))

(defgeneric busy-forget (toplevel))

(defgeneric widget-path (widget))

(defmethod widget-path ((w (eql nil))) nil)

(defmethod widget-path ((widget widget))
  "retrieve the slot value widget-path, if not given, create it"
  (or (%widget-path widget)
      (prog1
          (setf (slot-value widget 'widget-path)
                (create-path (master widget) (name widget)))
        (create widget))))

(defgeneric create (w))

(defmethod create ((widget widget))
  (when (init-command widget)
    ;;(format t "creating: ~a~%" (init-command widget)) (finish-output)
    (format-wish (init-command widget) (widget-path widget))))

(defgeneric (setf command) (value widget))

(defgeneric (setf image) (value widget))

(defgeneric command (widget))

(defmethod command ((widget widget))
  (gethash (name widget) (wish-callbacks *wish*)))

(defgeneric lower (widget &optional other))

(defmethod lower ((widget widget) &optional other)
  (format-wish "lower ~a~@[ ~a~]" (widget-path widget) (and other (widget-path other))))

(defgeneric raise (widget &optional above))

(defmethod raise ((widget widget) &optional above)
  (format-wish "raise ~a~@[ ~a~]" (widget-path widget) (and above (widget-path above))))

(defgeneric canvas (w))

(defgeneric value (widget)
  (:documentation "reads the value of the variable associated with the widget"))

(defgeneric (setf value) (widget val))

(defgeneric text (widget)
  (:documentation "reads the value of the textvariable associated with the widget"))

(defgeneric (setf text) (val variable))

;;; grab functions

(defgeneric grab (toplevel &key global))

(defmethod grab ((toplevel widget) &key global)
  (format-wish "grab set ~:[~;-global~] ~a" global (widget-path toplevel))
  toplevel)

(defgeneric grab-release (toplevel))

(defmethod grab-release ((toplevel widget))
  (format-wish "grab release ~a" (widget-path toplevel))
  toplevel)

(defmethod busy-hold ((toplevel widget))
  (format-wish (tclize `(tk busy hold ,(widget-path toplevel))))
  toplevel)

(defmethod busy-forget ((toplevel widget))
  (format-wish (tclize `(tk busy forget ,(widget-path toplevel))))
  toplevel)

(defmacro with-busy ((widget) &body body)
  `(unwind-protect
        (progn
          (busy-hold ,widget)
          ,@body)
     (busy-forget ,widget)))

;;; with-widget stuff

(defargs widget ()
  class cursor style takefocus)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun process-layout (line parent)
    (let ((class-name (first line))
          (instance-name (second line)))
      (multiple-value-bind (keyargs subwidgets)
          (do ((params (cddr line))     ; all other parameters to the widget/subwidget defs
               (keywords+values nil)    ; keyword args for the widget
               (sublists nil))          ; list of the subwidgets
              ((null params) (values (reverse keywords+values) (reverse sublists)))
            (cond ((listp (car params))
                   (dolist (subwidget (process-layout (pop params) instance-name))
                     (push subwidget sublists)))
                  (t (push (pop params) keywords+values)
                     (push (pop params) keywords+values))))
        (cons
         (list instance-name
               (append
                (list 'make-instance (list 'quote class-name))
                (if parent (list :master parent) nil)
                keyargs))
         subwidgets))))

  (defmacro with-widgets (layout &rest body)
    (let* ((defs (process-layout layout nil))
           (widgets (mapcar #'car defs)))
      `(let* ,defs
         (declare (ignorable ,@widgets))
         ,@body))))

;; defwidget the better version :)
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro defwidget (namespec parent slots widgetspecs &rest body)
    (let* ((name (if (listp namespec)
                     (second namespec)
                     namespec))
           (selfname (if (listp namespec)
                         (first namespec)
                         'self)))
      (unless name
        (error "defwidget: no name given"))

      (unless (listp parent)
        (error "defwidget: parent class(es) specifier \"~a\" needs to be a list of symbols" parent))

      (unless (listp slots)
        (error "defwidget: slots \"~a\" needs to be a list of slot specifiers" slots))

      (unless (listp widgetspecs)
        (error "defwidget: widgets \"~a\" need to be a list of widget specifiers" widgetspecs))

      (when (null widgetspecs)
        ;; (warn "defwidget: widget list is empty.")
        )

      (let (defs wnames events accessors methods)
        (labels ((on-type (subwidget methodname)
                   "Handle an :on-type form"
                   (push
                    `(bind ,subwidget "<KeyPress>"
                           (lambda (event)
                             (,methodname ,selfname (event-char event))))
                    events)
                   (push `(declaim (ftype function ,methodname))
                         methods)
                   ;;            (push
                   ;;             `(defgeneric ,methodname (self key))
                   ;;             methods)
                   ;;            (push
                   ;;             `(defmethod ,methodname ((,selfname ,name) keycode))
                   ;;             methods)
                   )

                 (process-layout (line parent)
                   (let ((instance-name (first line))
                         (class-name (second line)))
                     (multiple-value-bind (keyargs subwidgets)
                         (do ((params (cddr line))       ; all other parameters to the
                                        ; widget/subwidget defs
                              (keywords+values nil)      ; keyword args for the widget
                              (sublists nil))          ; list of the subwidgets
                             ((null params) (values (reverse keywords+values) (reverse sublists)))
                           (cond ((listp (car params))
                                  (dolist (subwidget (process-layout (pop params) instance-name))
                                    (push subwidget sublists)))
                                 ((equal (car params) :on-type)
                                  (pop params)
                                  (on-type instance-name (pop params)))
                                 (t (let* ((param (pop params))
                                           (val (pop params)))
                                      (push param keywords+values)
                                      (push (if (equal param :pack)
                                                (list 'quote val)
                                                val)
                                            keywords+values)))))
                       (cons
                        (list instance-name
                              (append
                               (list 'make-instance (list 'quote class-name))
                               (if parent (unless (member :master keyargs)
                                            (list :master parent)) nil)
                               keyargs))
                        subwidgets))))

                 (compute-slots (names)
                   (mapcar (lambda (name)
                             (if (listp name)
                                 name
                                 `(,name :accessor ,name :initform nil
                                         :initarg ,(intern (symbol-name name) :keyword))))
                           names))

                 (make-accessor (acname spec)
                   (push `(declaim (ftype function ,acname (setf ,acname)))
                         accessors)
                   (push
                    `(defmethod ,acname ((,selfname ,name))
                       ,spec)
                    accessors)
                   (push
                    `(defmethod (setf ,acname) (val (,selfname ,name))
                       (setf ,spec val))
                    accessors))

                 (grab-accessors ()
                   (loop while (equal (caar body) :accessor)
                         do (destructuring-bind (unused aname aspec)
                                (pop body)
                              (declare (ignore unused))
                              (make-accessor aname aspec)))))

          (setf defs (mapcan (lambda (w)
                               (process-layout w selfname)) widgetspecs))
          (setf wnames (mapcar #'car defs))
          (grab-accessors)
          (let* ((all-slots (compute-slots (append slots wnames))))
            `(progn
               (defclass ,name ,parent
                 ,all-slots)
               ,@(reverse accessors)
               ,@(reverse methods)

               (defmethod initialize-instance :after ((,selfname ,name) &key)
                 (let ,wnames
                   ,@(mapcar (lambda (def)
                               (append (list 'setf) def)) defs)
                   ,@(mapcar (lambda (wname)
                               `(setf (,wname ,selfname) ,wname)) wnames)

                   ,@events
                   ,@body)))))))))
