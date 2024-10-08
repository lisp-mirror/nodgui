;; This software is Copyright © cage
;; Portions Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; Nota  bene:  This system  definition  will  build a  stripped  down
;; version of nodgui that removes some dependencies related to fast 2D
;; and 3D rendering and also drops  support for JPEG file format, this
;; version of  the library can  be useful  if there are  problems with
;; CFFI dependencies.

(defsystem nodgui-lite
  :name             "nodgui-lite"
  :version          "0.7.1.3"
  :author           "cage"
  :encoding         :utf-8
  :maintainer       "cage"
  :bug-tracker      "https://codeberg.org/cage/nodgui/issues"
  :licence          "LLGPL"
  :description      "Lisp bindings for the Tk toolkit, lite version"
  :pathname         "src"
  :serial           t
  :depends-on (:alexandria
               :cl-unicode
               :cl-ppcre-unicode
               :esrap
               :parse-number
               :cl-colors2
               :named-readtables
               :pngload
               :zpng
               :flexi-streams
               :bordeaux-threads
               :static-vectors
               :trivial-garbage
               #-asdf3 :uiop)
  :components ((:file "package")
               (:file "config")
               (:file "constants")
               (:file "typed-operations")
               (:file "utils")
               (:file "synchronized-queue")
               (:file "non-blocking-queue")
               (:file "base64")
               (:file "ubvec4")
               (:file "vec2")
               (:file "vec3")
               (:file "matrix")
               (:file "fit-line")
               (:file "pixmap")
               (:file "sanitize")
               (:file "event-symbols")
               (:file "conditions")
               (:file "tcl-emitter")
               (:file "tcl-glue-code")
               (:file "event-parser")
               (:file "syntax")
               (:file "wish-communication")
               (:file "widget-helpers")
               (:file "widget")
               (:file "events")
               (:file "nodgui")
               (:file "photo-image")
               (:file "tkvariable")
               (:file "tktextvariable")
               (:file "treeview")
               (:file "button")
               (:file "checkbutton")
               (:file "radiobutton")
               (:file "scrollbar")
               (:file "combobox")
               (:file "entry")
               (:file "label")
               (:file "labelframe")
               (:file "listbox")
               (:file "menu")
               (:file "menubutton")
               (:file "menucheckbutton")
               (:file "menuradiobutton")
               (:file "message")
               (:file "notebook")
               (:file "panedwindow")
               (:file "progressbar")
               (:file "scale")
               (:file "separator")
               (:file "sizegrip")
               (:file "spinbox")
               (:file "text")
               (:file "frame")
               (:file "canvas")
               (:file "canvas-shapes")
               (:file "toplevel")
               (:file "dialog")
               (:file "error-handling")
               (:file "styles")
               (:file "wm")
               (:file "winfo")
               (:file "trivial-debugger")
               (:file "nodgui-mw")
               (:module tklib
                        :components ((:file "package")
                                     (:file "calendar")
                                     (:file "notify-window")
                                     (:file "plot")
                                     (:file "swaplist")
                                     (:file "misc-widget")))
               (:file "demo-tests")))

(pushnew :nodgui-lite *features*)

(pushnew :optimize-nodgui *features*)
