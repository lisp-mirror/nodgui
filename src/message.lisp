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

(a:define-constant +message-box-icons+ '("error" "info" "question" "warning")
  :test #'equalp
  :documentation "icon names valid for message-box function")

(defmacro gen-message-box-icons ()
  `(progn
     ,@(loop for icon in +message-box-icons+ collect
             `(a:define-constant ,(format-fn-symbol t "+message-box-icon-~a+" icon)
                ,icon :test #'string=))))

(gen-message-box-icons)

(a:define-constant +message-box-types+ '("abortretryignore"
                                         "ok"
                                         "okcancel"
                                         "retrycancel"
                                         "yesno"
                                         "yesnocancel")
  :test #'equalp)

(defmacro gen-message-box-type ()
  `(progn
     ,@(loop for type in +message-box-types+ collect
             `(a:define-constant ,(format-fn-symbol t "+message-box-type-~a+" type)
                ,type :test #'string=))))

(gen-message-box-type)

(defun message-box (message title type icon &key parent)
  ;;; tk_messageBox function
  (format-wish "senddatastring [tk_messageBox -message \"~a\" -title {~a} -type {~(~a~)} -icon {~(~a~)}~@[ -parent ~a~]]" message title type icon (and parent (widget-path parent)))
  (read-keyword))

(defun ask-yesno (message &key (title "") parent)
  (equal (message-box message title "yesno" "question" :parent parent) :yes))

(defun ask-okcancel (message &key (title "") parent)
  (equal (message-box message title "okcancel" "question" :parent parent) :ok))

(defun do-msg (message &key (title "") parent)
  (message-box message title "ok" "info" :parent parent))
