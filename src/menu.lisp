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

(defargs menu ()
  activebackground
  activeborderwidth
  activeforeground
  background
  borderwidth
  cursor
  disabledforeground
  font
  foreground
  postcommand
  relief
  selectcolor
  takefocus
  tearoff
  tearoffcommand
  title
  type)

(defclass menu (widget)
  ((text :accessor text
         :initarg :text)
   (help :accessor menu-help
         :initarg  :help
         :initform nil)))

;(defmethod create ((m menu))

(defmethod initialize-instance :after ((m menu) &key underline (tearoff 0))
  ;; TODO investigate removing
  (when (menu-help m) ;; special treatment for help menu
    (setf (name m) "help")
    (setf (slot-value m 'widget-path) (create-path (master m) (name m))))
  (format-wish "menu ~A -tearoff {~a}" (widget-path m) tearoff)
  (when (master m)
    (format-wish "~A add cascade -label {~A} -menu ~a ~@[ -underline {~a} ~]"
                 (widget-path (master m)) (text m) (widget-path m) underline)))

(defun make-menu (menu text &key underline name (tearoff 0))
  (if name
      (make-instance 'menu
                     :master    menu
                     :text      text
                     :underline underline
                     :name      name
                     :tearoff   tearoff)
      (make-instance 'menu
                     :master    menu
                     :text      text
                     :underline underline
                     :name      name
                     :tearoff tearoff)))

(defun add-separator (menu)
  (format-wish "~A add separator" (widget-path menu))
  menu)

(defgeneric state (menu menu-label state))

(defmethod state ((a menu) menu-label state)
  (format-wish "~a entryconfigure {~a} -state {~a}" (widget-path a) menu-label state))

(defgeneric menu-label (menu old new))

(defmethod menu-label ((a menu) old new)
  (format-wish "~a entryconfigure {~a} -label {~a}"  (widget-path a)  old new))

;;; window menu bar

(defclass menubar (widget) ())

(defun make-menubar (&optional (master nil))
 (make-instance 'menubar :master master :name "menubar"))

;(defmethod create ((mb menubar))
(defmethod initialize-instance :after ((mb menubar) &key)
  (format-wish "menu ~a -tearoff 0 -type menubar" (widget-path mb))
  (format-wish "~a configure -menu ~a" (if (master mb)
                                           (widget-path (master mb))
                                           (widget-path (root-toplevel)))
               (widget-path mb)))

;;; method to pop up a menue at the root window coordinates x and y

(defgeneric popup (menu x y))

(defmethod popup ((menu menu) x y)
  (format-wish "tk_popup ~A ~A ~A" (widget-path menu)
               (tk-number x) (tk-number y))
  menu)

(defgeneric menu-delete (menu index &optional end))

(defmethod menu-delete ((menu menu) index &optional (end :end))
  (format-wish "~A delete {~a} ~@[ {~(~A~)}~]" (widget-path menu) index (down end))
  menu)
