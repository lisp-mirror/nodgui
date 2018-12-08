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

(defclass menucheckbutton(menuentry)
  ((command
    :accessor command
    :initarg :command
    :initform nil)))

(defmethod initialize-instance :after ((m menucheckbutton) &key)
  (when (command m)
    (add-callback (name m) (command m)))
  (format-wish "~A add checkbutton -label {~A} -variable ~a ~@[ -command {callback ~a}~]"
               (widget-path (master m)) (text m) (name m) (and (command m) (name m))))

(defmethod value ((cb menucheckbutton))
  (format-wish "global ~a; senddata $~a" (name cb) (name cb))
  (read-data))

(defmethod (setf value) (val (cb menucheckbutton))
  (format-wish "global ~a; set ~a ~a" (name cb) (name cb) val)
  val)
