;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2019 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(defpackage :nodgui.tklib.calendar
  (:use :cl
        :alexandria
        :cl-ppcre
        :nodgui.event-parser
        :nodgui.sanitize
        :nodgui.tcl-emitter
        :nodgui.utils
        :nodgui)
  (:shadow :alexandria :rotate)
  (:export
   :calendar
   :make-calendar
   :set-date
   :set-date*
   :parse-selected-date)
  (:documentation "Wrapper for Calendar in tklib (https://core.tcl-lang.org/tklib/home).
Tklib *must* be installed on a system for this wrapper to works" ))
