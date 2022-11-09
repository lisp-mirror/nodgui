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

(in-package :test-event-parser)

(defsuite event-parser-suite (all-suite))

(defmacro compare-events (event output)
  `(assert-equality #'string=
       ,output
       (nth-value 0 (parse-event ,event))))

(deftest test-event-1 (event-parser-suite)
  (let ((*check-more-parsing-errors* nil))
    (compare-events "<1>"
                    "<1>")))

(deftest test-event-2 (event-parser-suite)
  (let ((*check-more-parsing-errors* nil))
    (compare-events "<a-b-c-dhhh h>        <e-f> <yn-iiiii>"
                    "<a-b-c-dhhh h><e-f><yn-iiiii>"))
  (compare-events "<Alt-Control-space>  <2> <3>"
                 "<Alt-Control-space><2><3>"))

(deftest test-event-field-has-spaces (event-parser-suite)
  (assert-condition nodgui-event-field-has-space
      (parse-event "<Mod1-Mod 2-Key-c>")))

(deftest test-event-invalid-detail (event-parser-suite)
  (assert-condition nodgui-event-invalid-detail
      (parse-event "<Mod1-Mod2-Key-cs>")))

(deftest test-event-invalid-modifier-second (event-parser-suite)
  (assert-condition nodgui-event-invalid-modifier
      (parse-event "<Mod1-Mod8-Key-c>")))

(deftest test-event-invalid-modifier-first (event-parser-suite)
  (assert-condition nodgui-event-invalid-modifier
      (parse-event "<Mo2d1-Mod1-Key-c>")))

(deftest test-event-invalid-type (event-parser-suite)
  (assert-condition nodgui-event-invalid-field
      (parse-event "<Mod1-Mod2-Keey-c>")))

(deftest test-event-incompatible-detail-with-button (event-parser-suite)
  (assert-condition nodgui-event-incompatible-type-detail
      (parse-event "<Mod1-Mod2-Button-c>")))

(deftest test-event-duplicate-modifier (event-parser-suite)
  (assert-condition nodgui-event-duplicate-modifier
      (parse-event "<Mod1-Mod1-Button-1>")))

(deftest test-event-duplicate-modifier-2 (event-parser-suite)
  (assert-condition nodgui-event-duplicate-modifier
      (parse-event "<Mod1-Mod2-Mod3-Mod2-Button-1>")))

(deftest test-event-duplicate-modifier-2 (event-parser-suite)
  (assert-condition nodgui-event-duplicate-modifier
      (parse-event "<Mod1-Mod2-Mod3-Mod4-Mod1-1>")))
