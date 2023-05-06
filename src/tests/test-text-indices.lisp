;; This software is Copyright © 2023 cage

;; The authors grant you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :test-text-indices)

(defsuite text-indices-suite (all-suite))

(defun parse (a)
  (nodgui:parse-indices a))

(deftest test-simple-line-char (text-indices-suite)
  (assert-equality #'string=
      "3.0"
      (parse '(:line 3 :char 0)))
  (assert-equality #'string=
      "3.0"
      (parse '(:char 0 :line 3)))
  (assert-condition error
      (parse '(:char 0 :char 3)))
  (assert-condition error
      (parse '(:line 0 :line 3)))
  (assert-condition error
      (parse '(:char 0 :line "a"))))

(deftest test-line-char (text-indices-suite)
  (assert-equality #'string=
      "3.0 +14 chars"
      (parse '(+ (:line 3 :char 0) 14 :chars)))
  (assert-equality #'string=
      "end +14 chars"
      (parse '(+ :end 14 :chars)))
  (assert-equality #'string=
      "3.end -3 chars"
      (parse '(- (:line 3 :char :end) 3 :chars)))
  (assert-condition error
      (parse '(+ (:line 3 :char 0) 14 :atoms)))
  (assert-condition error
      (parse '(* (:line 3 :char 0) 14 :chars)))
  (assert-condition error
      (parse '(+ (:line 3 :char 0) :chars))))

(deftest test-simple-xy (text-indices-suite)
  (assert-equality #'string=
      "@3,0"
      (parse '(:x 3 :y 0)))
  (assert-equality #'string=
      "@3,0"
      (parse '(:y 0 :x 3)))
  (assert-condition error
      (parse '(:x 0 :x 3)))
  (assert-condition error
      (parse '(:y 0 :y 3)))
  (assert-condition error
      (parse '(:x "r" :y 3))))

(deftest test-xy-submodifier (text-indices-suite)
  (assert-equality #'string=
      "@3,0 +10 display chars"
      (parse '(+ (:x 3 :y 0) +10 :chars :display)))
  (assert-condition error
      (parse '(+ (:x 3 :y 0) +10 :chars :invalid))))

(deftest test-tag-coordinates (text-indices-suite)
  (assert-equality #'string=
      "3.last +10 lines"
      (parse '(+ (:tag "3" :last) 10 :lines)))
  (assert-condition error
      (parse '(+ (:tag "3" :invalid) 10 :lines)))
  (assert-condition error
      (parse '(+ (:tag "3" :last :first) 10 :lines))))
