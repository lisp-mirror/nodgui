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

(defun tokenize-fn (input)
  (let* ((tokenizer-fn (lexer input)))
    tokenizer-fn))

(defun token-list-eq (l &rest expected)
  (every 'eq l expected))

(defmacro with-tokenizer (input &body body)
  (with-gensyms (tokenizer-fn)
    `(let ((,tokenizer-fn (lexer ,input)))
       (flet ((tok ()
                (funcall ,tokenizer-fn)))
         ,@body))))

(defun tokenize-all (str)
  (with-tokenizer str
    (flet ((tokn ()
             (second (multiple-value-list (tok)))))
      (do ((tok (tokn) (tokn))
           (res '()    (append res (list tok))))
           ((null tok) res)))))

(deftest test-lexer (event-parser-suite)
  (assert-true
      (every #'string=
             (tokenize-all "<a>")
             (list +delim-left+
                   "a"
                   +delim-right+)))
  (assert-true
      (every #'string=
             (tokenize-all "<a-b> ")
             (list +delim-left+ "a" +delim-field+ "b" +delim-right+)))
  (assert-true
      (every #'string=
             (tokenize-all "<a-b-c>")
             (list +delim-left+ "a" +delim-field+ "b" +delim-field+ "c" +delim-right+)))
  (assert-true
      (every #'string=
             (tokenize-all "<aa-bf-cg-dut>")
             (list +delim-left+ "aa" +delim-field+ "bf" +delim-field+ "cg" +delim-field+
                   "dut" +delim-right+)))
  (assert-true
      (every #'string=
             (tokenize-all "<aa-bf-cg-du t>")
             (list +delim-left+ "aa" +delim-field+ "bf" +delim-field+ "cg" +delim-field+
                   "du t" +delim-right+)))
  (assert-true
      (every #'string=
             (tokenize-all "<a-b-c-d> <e-f>")
             (list :delim-left  "a"     :delim-field "b" :delim-field "c" :delim-field "d"
                   :delim-right :filler :delim-left  "e" :delim-field "f" :delim-right)))
  (assert-true
      (every #'string=
             (tokenize-all "<<uiop>>")
             (list :delim-left :delim-left "uiop" :delim-right :delim-right))))

(defmacro compare-events (event output)
  `(assert-true
       (tree-equal (parse-event ,event)
                  ,output
                  :test #'string=)))

(deftest test-event-1 (event-parser-suite)
  (let ((*check-more-parsing-errors* nil))
    (compare-events "<1>"
                    '"<1>")))

(deftest test-event-2 (event-parser-suite)
  (let ((*check-more-parsing-errors* nil))
    (compare-events "<a-b-c-dhhh h>        <e-f> <yn-iiiii>"
                    "<a-b-c-dhhh h><e-f><yn-iiiii>")))

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
