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

(defclass tkvariable ()
  ((data-returned-are-list
    :initform nil
    :initarg  :data-returned-are-list-p
    :reader   data-returned-are-list-p
    :writer   (setf data-returned-are-list))))

(defmethod initialize-instance :around ((v tkvariable)
                                        &key
                                          (initial-value nil)
                                          (use-configure-subcommand-p t)
                                          &allow-other-keys)
  (call-next-method)
  (if use-configure-subcommand-p
      (format-wish "~a configure -variable ~a ; global ~a ; set ~a {}"
                   (widget-path v) (name v) (name v) (name v))
      (format-wish "global ~a ; set ~a {}"
                   (name v) (name v)))
  (when initial-value
    (setf (value v) initial-value)))

(defmethod value ((v tkvariable))
  (format-wish "global ~a; senddata $~a" (name v) (name v))
  (if (data-returned-are-list-p v)
      (read-data :expected-list-as-data t)
      (read-data)))
