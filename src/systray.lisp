;; This software is Copyright © cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui)

;; tk systray configure ?option? ?value option value ...?
;; tk systray exists
;; tk systray destroy

(defgeneric systray-create (image &key text button-1-callback button-3-callback))

(a:define-constant +systray-command+ "tk systray " :test #'string=)

(defun systray-exists-p ()
  (tcl-bool->lisp (with-read-data ()
                    (format-wish (tclize `(senddata [+ ,+systray-command+ exists ]))))))

(defun systray-destroy ()
  (format-wish (tclize `(,+systray-command+ destroy))))

(defun %systray-create (image text button-1-callback button-3-callback)
  (assert (functionp button-1-callback))
  (assert (functionp button-3-callback))
  (assert (or (stringp text)
              (null text)))
  (format-wish (tclize `(,+systray-command+
                         create -image ,(name image) " "
                         ,(empty-string-if-nil text
                            `(-text  \"+ ,text \" " "))
                         -button1 {+ callback ,(install-callback button-1-callback) }
                           -button3 {+ callback ,(install-callback button-3-callback) }))))

(defmethod systray-create ((image pixmap)
                           &key
                             (text nil)
                             (button-1-callback (constantly t))
                             (button-3-callback (constantly t)))
  (let ((photo (make-image image)))
    (%systray-create photo text button-1-callback button-3-callback)))

(defmethod systray-create ((image string)
                           &key
                             (text nil)
                             (button-1-callback (constantly t))
                             (button-3-callback (constantly t)))
    "If object is a valid local file path is threated as a  pathname, otherwise the
string must  be base64 encoded image.  Note in both cases  only PNG or
GIF are supported but if tk-img is used more format are available!"
  (let ((photo (make-image image)))
    (%systray-create photo text button-1-callback button-3-callback)))

(defmethod systray-create ((image vector)
                           &key
                             (text nil)
                             (button-1-callback (constantly t))
                             (button-3-callback (constantly t)))
  (let ((photo (make-image image)))
    (%systray-create photo text button-1-callback button-3-callback)))

(defgeneric sysnotify (title text))

(defmethod sysnotify ((title string) (message string))
  (format-wish (tclize `("tk sysnotify " \"+,title \" \"+,message \"))))

;; demo

(defparameter *systray-icon-dir* (uiop:native-namestring (asdf:system-relative-pathname :nodgui "game/data"
                                                                                          :type :directory)))

(defparameter *systray-icon* (strcat *systray-icon-dir* "player.png"))

(defun systray-test ()
  ;(setf *debug-tk* t)
  (with-nodgui ()
    (systray-create *systray-icon*)
    (sysnotify "test" "Notification from systray")))
