;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2018,2019,2020,2021,2022,2023 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui)

(defstruct event
  x              ; 0
  y              ; 1
  char-code      ; 2
  keycode        ; 3
  char           ; 4
  width          ; 5
  height         ; 6
  root-x         ; 7
  root-y         ; 8
  mouse-button   ; 9
  unicode-char   ; 10
  timestamp      ; 11
  delta          ; 12
  others)        ; 13

(defun construct-tk-event (properties)
  "create an event structure from a list of values as read from tk"
  (make-event :x            (first properties)                ; 0
              :y            (second properties)               ; 1
              :char-code    (third properties)                ; 2
              :keycode      (fourth properties)               ; 3
              :char         (fifth properties)                ; 4
              :width        (sixth properties)                ; 5
              :height       (seventh properties)              ; 6
              :root-x       (eighth properties)               ; 7
              :root-y       (ninth properties)                ; 8
              :mouse-button (tenth properties)                ; 9
              :unicode-char (elt   properties 10)             ; 10
              :timestamp    (let ((data (elt properties 11))) ; 11
                              (if (numberp data)
                                  data
                                  nil))
              :delta        (let ((data (elt properties 12))) ; 12
                              (if (numberp data)
                                  data
                                  nil))
              :others       (if (or (null (elt properties 13))
                                    (and (stringp (elt properties 13))
                                         (string= (elt properties 13)  ; 13
                                                  "")))
                                nil
                                (elt properties 13))))

(defun event-user-data (event)
  (event-others event))

(defgeneric bind (w event fun &key append exclusive))

(defmethod bind ((object widget) event fun &key append exclusive)
  "bind fun to event of the widget object"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "bind  ~a {~a} {~:[~;+~]sendevent ~A %x %y %N %k %K %w %h %X %Y %b %A %t %D %d ~:[~;;break~]}"
                 (widget-path object) event append name exclusive)
    object))

(defmethod bind ((object string) event fun &key append exclusive)
  "bind fun to event within context indicated by string ie. 'all' or 'Button'"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "bind  {~a} {~a} {~:[~;+~]sendevent ~A %x %y %N %k %K %w %h %X %Y %b %A %t %D %d ~:[~;;break~]}"
                 object event append name exclusive)
    object))

(defgeneric unbind (object event))

(defmethod unbind ((object widget) event)
  "bind fun to event of the widget object"
  (unbind (widget-path object) event)
  object)

(defmethod unbind ((object string) event)
  "bind fun to event of the widget object"
    (format-wish "bind ~a {~a} \"\"" object event)
    object)

(defparameter *debounce-minimum-delay* 120
  "milliseconds")

(defun parse-lambda-debouce-body (body)
  (let* ((declaration-position (position-if (lambda (a)
                                              (and (listp a)
                                                   (eq (first a) 'declare)))
                                            body))
         (declarations         (and declaration-position
                                    (subseq body 0 (1+ declaration-position))))
         (actual-body          (if declaration-position
                                   (subseq body (1+ declaration-position))
                                   body)))
    (values declarations actual-body)))

(defmacro lambda-debounce (args &body body)
  (multiple-value-bind (declarations actual-body)
      (parse-lambda-debouce-body body)
    (a:with-gensyms (last-fired saved-last-fired fired-time results debounce-minimum-delay)
      `(let ((,last-fired (current-time-milliseconds))
             (,debounce-minimum-delay *debounce-minimum-delay*))
         (lambda ,args
           ,@declarations
           (let ((,fired-time (current-time-milliseconds))
                 (,saved-last-fired ,last-fired)
                 (,results nil))
             (when (> (- ,fired-time ,saved-last-fired)
                      ,debounce-minimum-delay)
               (setf ,results (progn ,@actual-body)))
             (setf ,last-fired (current-time-milliseconds))
             ,results))))))

(defparameter *lambda-frequency* 120
  "milliseconds")

(defmacro lambda-fixed-frequency (args &body body)
  (multiple-value-bind (declarations actual-body)
      (parse-lambda-debouce-body body)
    (a:with-gensyms (last-fired saved-last-fired fired-time results lambda-frequency)
      `(let ((,last-fired (current-time-milliseconds))
             (,lambda-frequency *lambda-frequency*))
         (lambda ,args
           ,@declarations
           (let ((,fired-time (current-time-milliseconds))
                 (,saved-last-fired ,last-fired)
                 (,results nil))
             (when (> (- ,fired-time ,saved-last-fired)
                      ,lambda-frequency)
               (setf ,results (progn ,@actual-body))
               (setf ,last-fired ,fired-time))
             ,results))))))

(defun add-event-alias (virtual-event &rest events)
  (assert events)
  (let ((*suppress-newline-for-tcl-statements* t))
    (format-wish (tclize `(event add
                                 ,virtual-event " "
                                 ,(reduce (lambda (a b) (format nil "~a ~a" a b))
                                          events))))))

(defun remove-event-alias (virtual-event &rest events)
  (format-wish (tclize `(event delete ,virtual-event " "
                               ,(reduce (lambda (a b) (format nil "~a ~a" a b))
                                        events
                                        :initial-value "")))))

(a:define-constant +legals-event-options+
    '(:when
      :above
      :borderwidth
      :button
      :count
      :data
      :delta
      :detail
      :focus
      :height
      :keycode
      :keysym
      :mode
      :override
      :place
      :root
      :rootx
      :rooty
      :sendevent
      :serial
      :state
      :subwindow
      :time
      :warp
      :width
      :window
      :x
      :y)
  :test #'equalp)

(defun fire-event (window virtual-event &rest options)
  (setf options
        (loop for element in options
              for ct from 1
              collect (if (oddp ct)
                          (cond
                            ((member element '(:others :user-data) :test #'eq)
                             :data)
                            ((member element +legals-event-options+)
                             element)
                            (t
                             (error "Illegal event option value: ~s" element)))
                          element)))
  (let ((*suppress-newline-for-tcl-statements* t))
    (format-wish (tclize `(event generate
                                 ,(widget-path window) " "
                                 ,virtual-event " "
                                 ,@(loop for (option value) on options by 'cddr
                                         collect `(,(format nil "-~(~a~)" option)
                                                       " "
                                                   \"+ ,(format nil "~s" value) \")))))))
(defun event-alias-info (virtual-event)
  (with-read-data ()
    (format-wish "senddatastring [event info {~a}]" virtual-event)))
