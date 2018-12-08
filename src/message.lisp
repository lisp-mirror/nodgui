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

(defargs message ()
  anchor
  aspect
  background
  borderwidth
  cursor
  font
  foreground
  highlightbackground
  highlightcolor
  highlightthickness
  justify
  padx
  pady
  relief
  takefocus
  textvariable
  width)

(defwrapper message (tktextvariable widget) () "message")

(defvar *mb-icons* (list "error" "info" "question" "warning")
  "icon names valid for message-box function")

;;; see make-string-output-string/get-output-stream-string
(defun message-box (message title type icon &key parent)
  ;;; tk_messageBox function
  (format-wish "senddatastring [tk_messageBox -message \"~a\" -title {~a} -type ~(~a~) -icon ~(~a~)~@[ -parent ~a~]]" (tkescape2 message) title type icon (and parent (widget-path parent)))
  (read-keyword))

(defun ask-yesno(message &key (title "") parent)
  (equal (message-box message title "yesno" "question" :parent parent) :yes))

(defun ask-okcancel(message &key (title "") parent)
  (equal (message-box message title "okcancel" "question" :parent parent) :ok))

(defun do-msg(message &key (title "") parent)
  (message-box message title "ok" "info" :parent parent))
