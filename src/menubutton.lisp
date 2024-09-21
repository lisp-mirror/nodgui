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

(defargs menubutton ()
  class
  compound
  cursor
  direction
  image
  menu
  state
  style
  takefocus
  textvariable
  underline
  width)

(defclass menuentry (widget)
  ((text
    :accessor text
    :initarg :text
    :initform ""))
  (:documentation  "An abstract  base class  for menu  entries.  These
\"widgets\"   have  to   be   handled  specially   by  some   methods,
e.g. 'configure'."))

(defclass menubutton (menuentry) ())

(defmethod initialize-instance :after ((m menubutton) &key command underline accelerator state)
  (when command
    (add-callback (name m) command))
  (format-wish "~A add command -label {~A}  -command {callback ~A}~@[ -underline {~a} ~]~@[ -accelerator {~a} ~]~@[ -state {~(~a~)}~]"
               (widget-path (master m)) (text m) (name m) underline accelerator state))

(defun make-menubutton (menu text command &key underline accelerator state)
  (let* ((mb (make-instance 'menubutton
                            :master      menu
                            :text        text
                            :command     command
                            :underline   underline
                            :accelerator accelerator
                            :state       state)))
    mb))

(defmethod configure ((item menuentry) option value &rest others)
  (let ((path (widget-path (master item))))
    (format-wish "~A entryconfigure [~A index {~A}]~{ {-~(~a~)} {~a}~}"
                 path
                 path
                 (text item)
                 (mapcar #'down (list* option value others))))
  item)
