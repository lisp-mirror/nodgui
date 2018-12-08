;; This software is Copyright © 2018 cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(defpackage :all-tests
  (:use :cl
        :clunit)
  (:export
   :all-suite
   :run-all-tests))

(defpackage :test-event-parser
  (:use :cl
        :alexandria
        :clunit
        :nodgui.config
        :nodgui.constants
        :nodgui.utils
        :nodgui.conditions
        :nodgui.event-parser
        :all-tests)
  (:export :event-parser-suite))

(defpackage :test-tcl-emitter
  (:use :cl
        :alexandria
        :clunit
        :nodgui.config
        :nodgui.constants
        :nodgui.utils
        :nodgui.conditions
        :nodgui.tcl-emitter
        :all-tests)
  (:export :tcl-emitter-suite))
