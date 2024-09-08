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

(in-package :nodgui.sanitize)

(defun escape-~ (text)
  (cl-ppcre:regex-replace-all "~" text "~~"))

(defgeneric %tkescape (object escapable-chars))

(defgeneric %tkescape (object escapable-chars))

(defmethod %tkescape ((object simple-string) (escapable-chars list))
  #.nodgui.config:default-optimization
  (loop with result = (make-string-buffer)
        for c across object do
          (when (member c escapable-chars)
            (vector-push-extend #\\ result))
          (vector-push-extend c result)
        finally (return result)))

(defmethod %tkescape (object (escapable-chars list))
  (%tkescape (format nil "~a" object) escapable-chars))

;; Much faster version. For one test run it takes 2 seconds, where the
;; other implementation requires 38 minutes.

(defun escape-braces (text)
  (%tkescape text '(#\{ #\})))

(defun tkescape (text)
  (%tkescape text '(#\\ #\$ #\[ #\] #\{ #\} #\")))

(defun tkescape2 (text)
  (%tkescape text '(#\\ #\$ #\[ #\] #\")))

;;; sanitizing strings: lisp -> tcl (format (wish-stream *wish*) "{~a}" string)
;;; in string escaped : {} mit \{ bzw \}  und \ mit \\

(defstruct protect-escape (data))

(defstruct (bypass-escape (:include protect-escape)))

(defstruct (escape-only-braces (:include protect-escape)))

(defun rem-trouble-chars-and-then-wrap (s)
  (make-protect-escape :data (sanitize-remove s)))

(defgeneric sanitize (object))

(defmethod sanitize (object)
  (sanitize (to-s object)))

(defmethod sanitize ((object number))
  "Return numbers unprocessed"
  object)

(defmethod sanitize ((object string))
  (if (string= object "{}")
      object
      (tkescape object)))

(defmethod sanitize ((object list))
  (map 'list #'sanitize object))

(defmethod sanitize ((object protect-escape))
  (format nil "{~a}" (protect-escape-data object)))

(defmethod sanitize ((object bypass-escape))
  (bypass-escape-data object))

(defmethod sanitize ((object escape-only-braces))
  (let* ((as-string       (to-s (escape-only-braces-data object)))
         (balanced-braces 0))
    (loop named balancing-test
          with first-closing = t
          for c across as-string do
      (cond
        ((char= c #\})
         (decf balanced-braces)
         (when first-closing
           (return-from balancing-test nil)))
        ((char= c #\{)
         (setf first-closing nil)
         (incf balanced-braces))))
    (if (= balanced-braces 0)
        as-string
        (escape-braces as-string))))

(defmethod sanitize ((object (eql nil)))
  nil)

(defgeneric sanitize-remove (object))

(defmethod sanitize-remove ((object (eql nil)))
  nil)

(defmethod sanitize-remove ((object string))
  (cl-ppcre:regex-replace-all "(\")|(\\[)|(\\])" object ""))

(defmethod sanitize-remove ((object list))
  (map 'list #'sanitize-remove object))
