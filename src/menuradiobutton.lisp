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

(defclass menuradiobutton (menuentry)
  ((command
    :accessor command
    :initarg :command
    :initform nil)
   (group
    :accessor group
    :initarg :group
    :initform nil)))

(defmethod initialize-instance :after ((m menuradiobutton) &key)
  (when (command m)
    (add-callback (name m) (command m)))
  (unless (group m)
    (setf (group m)
          (name m)))
  (format-wish "~A add radiobutton -label {~A} -value ~a -variable ~a ~@[ -command {callback ~a}~]"
               (widget-path (master m)) (text m) (name m) (group m)
               (and (command m) (name m))))

(defmethod value ((cb menuradiobutton))
  (format-wish "global ~a; senddata ${~a}" (group cb) (group cb))
  (read-data))

(defmethod (setf value) (val (cb menuradiobutton))
  (format-wish "global ~a; set ~a {~a}" (group cb) (group cb) val)
  val)
