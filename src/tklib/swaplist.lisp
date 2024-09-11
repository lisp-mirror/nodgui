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

(in-package :nodgui.tklib.swaplist)

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(a:define-constant +swaplist-library-name+ "swaplist" :test #'string=)

(defclass swaplist (widget tkvariable) ())

(defmethod initialize-instance :after ((object swaplist)
                                       &key
                                         (left-list             '())
                                         (right-list            '())
                                         (left-list-label       "")
                                         (right-list-label      "")
                                         (to-right-button-label ">>")
                                         (to-left-button-label  "<<")
                                         (to-up-button-label    "^")
                                         (to-down-button-label  "v")
                                       &allow-other-keys)
  (require-tcl-package +swaplist-library-name+)
  (with-accessors ((widget-path widget-path)
                   (name        name)) object
    (let ((actual-left-list  (wrap-with-double-quotes left-list))
          (actual-right-list (wrap-with-double-quotes right-list)))
      (with-no-emitted-newline
        (format-wish (tclize `("::swaplist::swaplist "
                               ,widget-path  " "
                               ,nodgui::name " "
                               [list ,actual-left-list  ]
                               [list ,actual-right-list ]
                               -embed
                               -reorder     true
                               -llabel      \"+ ,left-list-label       \"
                               -rlabel      \"+ ,right-list-label      \"
                               -lbuttontext \"+ ,to-left-button-label  \"
                               -rbuttontext \"+ ,to-right-button-label \"
                               -dbuttontext \"+ ,to-down-button-label  \"
                               -ubuttontext \"+ ,to-up-button-label    \"))))))
  object)

(defun make-swaplist (left-list right-list
                      &key
                        (left-list-label       "")
                        (right-list-label      "")
                        (to-right-button-label (double-right-arrow))
                        (to-left-button-label  (double-left-arrow))
                        (to-up-button-label    (up-arrow))
                        (to-down-button-label  (down-arrow)))
  "Make a  widget that permits  to swap values between  two listboxes,
  the values of the list on the right can be accessed using the 'value' method.

- left-list-label        the label for the left list
- right-list-label       the label for the right list
- to-right-button-label  the label for button that moves elements from left to right
- to-left-button-label   the label for button that moves elements from right to left
- to-up-button-label     the label for button that move elements, of the right list, upper
- to-down-button-label   the label for button that move elements, of the right list, down
"
  (make-instance 'swaplist
                 :left-list                  left-list
                 :right-list                 right-list
                 :left-list-label            left-list-label
                 :right-list-label           right-list-label
                 :to-right-button-label      to-right-button-label
                 :to-left-button-label       to-left-button-label
                 :to-up-button-label         to-up-button-label
                 :to-down-button-label       to-down-button-label
                 :data-returned-are-list-p   t
                 :use-configure-subcommand-p nil))
