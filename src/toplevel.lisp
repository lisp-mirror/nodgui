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

(defargs toplevel ()
  borderwidth
  class
  menu
  relief
  screen
  use
  background
  colormap
  container
  cursor
  height
  highlightbackground
  highlightcolor
  highlightthickness
  padx
  pady
  takefocus
  visual
  width)

(defwrapper toplevel (widget)
  ((protocol-destroy :accessor protocol-destroy :initarg :on-close :initform nil)
   (title :accessor title :initform nil :initarg :title))
  "toplevel"
  (when (title widget)
    (wm-title widget (title widget)))
  (when (not (or (eq (protocol-destroy widget) :none)
                 (protocol-destroy widget)))
    (format-wish "wm protocol ~a WM_DELETE_WINDOW {wm withdraw ~a}" (widget-path widget) (widget-path widget))))

(defun make-toplevel (master)
  (make-instance 'toplevel :master master))

(defun window-x (tl)
  "give the x position of the toplevel in pixels"
  (root-x tl))

(defun window-y (tl)
  "give the y position of the toplevel in pixels"
  (root-y tl))

(defun window-transient (tl win)
  "set the transient property of tl to be transient to win or nil.
   Please use transient in wm.lisp instead"
  (format-wish "wm transient ~a ~a" (widget-path tl) (if win
                                                         (widget-path win)
                                                         "{}")))
