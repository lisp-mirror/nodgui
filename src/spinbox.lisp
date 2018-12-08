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

(defargs spinbox ()
  cursor
  state
  style
  takefocus
  validate
  validatecommand
  xscrollcommand
  command
  format
  from
  increment
  to
  values
  wrap)

(defwrapper spinbox (tktextvariable widget) () "ttk::spinbox")

(defmethod (setf command) (val (sp spinbox))
  (add-callback (name sp) val)
  (format-wish "~a configure -command {callbackstring ~a %s}" (widget-path sp) (name sp))
  val)
