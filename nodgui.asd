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

(defsystem nodgui
  :name             "nodgui"
  :version          "0.7.4.0"
  :author           "cage"
  :encoding         :utf-8
  :maintainer       "cage"
  :bug-tracker      "https://codeberg.org/cage/nodgui/issues"
  :licence          "LLGPL"
  :description      "Lisp bindings for the Tk toolkit"
  :pathname         "src"
  :serial           t
  :depends-on (:alexandria
               :cl-unicode
               :cl-ppcre-unicode
               :esrap
               :parse-number
               :cl-colors2
               :named-readtables
               :jpeg-turbo
               :pngload
               :zpng
               :flexi-streams
               :bordeaux-threads
               :sdl2
               :sdl2-ttf
               :cl-opengl
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
               (:file "thread-pool")
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
               (:file "systray")
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
               (:file "rendering-buffer-context")
               (:file "pixels-buffer")
               (:file "opengl-frame")
               (:module tklib
                :components ((:file "package")
                             (:file "calendar")
                             (:file "notify-window")
                             (:file "plot")
                             (:file "swaplist")
                             (:file "misc-widget")))
               (:file "demo-tests")
               (:file "demo-pixels-buffer")
               (:file "demo-3d-window")
               (:file "tcl-lib-wrapped")
               (:static-file "demo/shaders/3d-demo.frag")
               (:static-file "demo/shaders/3d-demo.vert"))
  :in-order-to ((test-op (test-op :nodgui/test))))

(defsystem nodgui/test
  :encoding         :utf-8
  :pathname         "test"
  :serial           t
  :depends-on (:nodgui
               :clunit2)
  :components ((:file "package")
               (:file "all-tests")
               (:file "test-non-blocking-queue")
               (:file "test-tcl-emitter")
               (:file "test-event-parser")
               (:file "test-styles")
               (:file "test-text-indices")
               (:file "test-nodgui"))
  :perform (test-op (op c) (symbol-call :all-tests :run-all-tests)))

(defsystem nodgui/game
  :encoding         :utf-8
  :pathname         "game"
  :serial           t
  :depends-on (:nodgui
               :clunit2)
  :components ((:file "package")
               (:file "aabb2")
               (:file "entities")
               (:file "world")
               (:file "main")))

(pushnew :nodgui *features*)

(pushnew :optimize-nodgui *features*)

;; (pushnew :debug-game *features*)

;; (pushnew :suppress-debug-messages *features*)
