;; This software is Copyright © 2022 cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :test-tk-styles)

(defsuite tk-styles-suite (all-suite))

(deftest parse-layout (tk-styles-suite)
  (let ((expected '("Button.border" :STICKY "nswe" :BORDER "1"
                    ("Button.focus" :STICKY "nswe"
                     ("Button.padding" :STICKY "nswe"
                      ("Button.label" :STICKY "nswe"
                       "red-corner.TButton" :SIDE "right" :STICKY "ne"))))))
    (assert-equality (lambda (a b) (tree-equal a b :test #'string=))
        (values expected nil t)
        (nodgui::parse-layout "Button.border -sticky nswe -border 1 -children {Button.focus -sticky nswe -children {Button.padding -sticky nswe -children {Button.label -sticky nswe red-corner.TButton -side right -sticky ne}}}"))))

(deftest emit-layout (tk-styles-suite)
  (let ((serialized "Button.border -sticky nswe -border 1 -children { Button.focus -sticky nswe -children { Button.padding -sticky nswe -children { Button.label -sticky nswe red-corner.TButton -side right -sticky ne }}}"))
    (assert-equality #'string=
        serialized
       (nodgui::list->layout (nodgui::parse-layout serialized)))))

(deftest insert-test ()
  (let ((parsed (nodgui::parse-layout "Button.border -sticky nswe -border 1 -children {Button.focus -sticky nswe -children {Button.padding -sticky nswe -children {Button.label -sticky nswe}}}"))
        (new-node (nodgui::parse-layout "red-corner.TButton -side right -sticky ne")))
    (assert-true
        (tree-equal (nodgui::insert-layout parsed new-node "Button.label")
                    '("Button.border" :sticky "nswe"
                      :border "1"
                      ("Button.focus" :sticky "nswe"
                       ("Button.padding" :sticky "nswe"
                        ("red-corner.TButton" :side "right" :sticky "ne"
                         "Button.label" :sticky "nswe"))))))))
