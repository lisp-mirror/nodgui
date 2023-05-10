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
  others)        ; 11

(defun construct-tk-event (properties)
  "create an event structure from a list of values as read from tk"
  (make-event
   :x            (first properties)               ; 0
   :y            (second properties)              ; 1
   :char-code    (third properties)               ; 2
   :keycode      (fourth properties)              ; 3
   :char         (fifth properties)               ; 4
   :width        (sixth properties)               ; 5
   :height       (seventh properties)             ; 6
   :root-x       (eighth properties)              ; 7
   :root-y       (ninth properties)               ; 8
   :mouse-button (tenth properties)               ; 9
   :unicode-char (elt   properties 10)            ; 10
   :others       (if (string= (elt properties 11) ; 11
                              "")
                     nil
                     (elt properties 11))))

(defgeneric bind (w event fun &key append exclusive))

(defmethod bind ((w widget) event fun &key append exclusive)
  "bind fun to event of the widget w"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "bind  ~a {~a} {~:[~;+~]sendevent ~A %x %y %N %k %K %w %h %X %Y %b %A ~:[~;;break~]}"
                 (widget-path w) event append name exclusive)
    w))

(defmethod bind (s event fun &key append exclusive)
  "bind fun to event within context indicated by string ie. 'all' or 'Button'"
  (let ((name (create-name)))
    (add-callback name fun)
    (format-wish "bind  {~a} {~a} {~:[~;+~]sendevent ~A %x %y %N %k %K %w %h %X %Y %b %A ~:[~;;break~]}"
                 s event append name exclusive)))

(defgeneric bind-debounce (w event fun &key append exclusive))

(defun calculate-internal-time-scaling-millis (&optional (scaling 1000))
  (if (<= (/ internal-time-units-per-second scaling)
          1000)
      scaling
      (calculate-internal-time-scaling-millis (* 10 scaling))))

(defparameter *internal-time-scaling-millis* (calculate-internal-time-scaling-millis))

(defparameter *debounce-minimum-delay* 300
  "milliseconds")

(defun calculate-milliseconds-elapsed ()
  (truncate (/ (get-internal-real-time)
               *internal-time-scaling-millis*)))

(defmacro lambda-debounce (args &body body)
  (a:with-gensyms (last-fired saved-last-fired now)
    `(let ((,last-fired (calculate-milliseconds-elapsed)))
       (lambda ,args
         (let ((,now (calculate-milliseconds-elapsed))
               (,saved-last-fired ,last-fired))
           (setf ,last-fired ,now)
           (when (> (- ,now ,saved-last-fired)
                    *debounce-minimum-delay*)
             ,@body))))))
