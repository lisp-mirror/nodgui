;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2021 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.utils)

(a:define-constant +base52-encode-table+
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  :test #'string=)

(defparameter *directory-sep-regexp*
  #+windows "\\"
  #-windows "\\/")

(defparameter *directory-sep*
  #+windows "\\"
  #-windows "/")

(defmacro format-fn-symbol (package format &rest format-args)
  `(a:format-symbol ,package ,(concatenate 'string "~:@(" format "~)")
                             ,@format-args))

(defun format-keyword (thing)
  (a:make-keyword (format nil "~:@(~a~)" thing)))

(defun to-s (v)
  (format nil "~a" v))

(defun strcat (&rest chunks)
  (strcat* chunks))

(defun strcat* (chunks)
  (with-output-to-string (stream)
    (loop for i in chunks do
         (write-sequence i stream))))

(defun string-empty-p (s)
  (or (null s)
      (string= s "")))

(defun wrap-with (s wrapper)
  (strcat wrapper s wrapper))

(defun strip-prefix (string prefix)
  (let ((re (strcat "^" prefix)))
    (cl-ppcre:regex-replace re string "")))

(defun strip-withespaces (string)
  (let ((re "\\s"))
    (cl-ppcre:regex-replace re string "")))

(defun common-prefix (&rest strings)
  (let* ((prefix-count   0)
         (sorted-strings (sort strings #'(lambda (a b) (> (length a) (length b)))))
         (pivot-string   (a:first-elt sorted-strings))
         (actual-strings (rest sorted-strings))
         (res            (string (a:first-elt pivot-string))))
    (labels ((advance-res ()
               (incf prefix-count)
               (setf res (strcat res (string (elt pivot-string prefix-count)))))
             (%advance ()
               (loop for i in actual-strings do
                    (when (not (cl-ppcre:scan (strcat "^" res) i))
                      (setf res (subseq res 0 (1- (length res))))
                      (return-from %advance nil)))
               (when (< (1+ prefix-count)
                        (length pivot-string))
                 (advance-res)
                 (%advance))))
      (%advance)
      res)))

(defgeneric join-with-strings (strings junction))

(defmethod join-with-strings ((strings list) junction)
  (reduce #'(lambda (a b) (strcat a junction b)) strings))

(defmethod join-with-strings ((strings string) junction)
  (declare (ignore junction))
  strings)

(defun join-with-strings* (junction &rest strings)
  (reduce #'(lambda (a b) (strcat a junction b)) strings))

(defun trim (s &optional (bag '(#\Space #\tab #\Newline #\Return)))
  (string-trim bag s))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun camel-case->snail-case (s &key (make-downcase t))
    (let ((res (cl-ppcre:regex-replace-all "(.)([A-Z])" s "\\1-\\2")))
      (if make-downcase
          (string-downcase res)
          res))))

(defun make-adjustable-string (&optional (string "") (fill-pointer t))
  (if string
      (make-array (length string)
                  :element-type     'character
                  :initial-contents string
                  :adjustable       t
                  :fill-pointer     fill-pointer)
      (make-array 512
                  :element-type     'character
                  :adjustable       t
                  :fill-pointer     0)))

(defun make-string-buffer (&optional (size 128))
  (make-array size
              :element-type     'character
              :adjustable       t
              :fill-pointer     0))

(defun to-stderr (control &rest things)
   (apply #'format (append (list *error-output* (strcat "[DEBUG] " control "~%"))
                           things)))

(defun encode-base-52 (value)
  (let ((result ""))
    (loop
       while (> value 0)
       do
          (multiple-value-bind (mul rest)
              (truncate value 52)
            (setf result (format nil "~a~a" result (elt +base52-encode-table+ rest)))
            (setf value mul)))
    result))

(defun split-sequence (string at)
  (let ((pos (search at string))
        erg)
    (loop
       while pos
       do
         (when (> pos 0)
           (push (subseq string 0 pos) erg))
         (setf string (subseq string (+ pos (length at))))
         (setf pos (search at string)))
    (when (> (length string) 0)
      (push string erg))
    (nreverse erg)))

(defun split-words (text)
  (split "\\p{White_Space}+" text))

(defgeneric delete@ (sequence position))

(defgeneric safe-delete@ (sequence position)
  (:documentation "Return sequence if position is out of bound"))

(defmacro gen-delete@ ((sequence position) &body body)
  `(if (and (>= ,position 0)
            (< ,position (length ,sequence)))
       ,@body
      (error 'nodgui.conditions:out-of-bounds :seq sequence :idx position)))

(defmethod delete@ ((sequence list) position)
  (gen-delete@
   (sequence position)
   (append (subseq sequence 0 position)
           (and (/= position (- (length sequence) 1))
                (subseq sequence (1+ position))))))

(defmethod delete@ ((sequence vector) position)
  (gen-delete@
   (sequence position)
    (make-array (1- (length sequence))
                :fill-pointer (1- (length sequence))
                :adjustable t
                :initial-contents (concatenate 'vector (subseq sequence 0 position)
                                               (and (/= position (- (length sequence) 1))
                                                    (subseq sequence (1+ position)))))))

(defmethod safe-delete@ ((sequence sequence) position)
  (restart-case
      (delete@ sequence position)
    (return-nil () nil)
    (return-whole () sequence)
    (new-index (i) (safe-delete@ sequence i))))

(defun make-fresh-list (size &optional (el nil))
  (map-into (make-list size)
            (if (functionp el)
                el
                #'(lambda () el))))

(defun make-array-frame (size &optional (el nil) (type t) (simplep nil))
  "All elements points to the same address/reference!"
  (make-array size
              :fill-pointer (if (not simplep) size nil)
              :adjustable (if (not simplep) t nil)
              :initial-element el
              :element-type type))

(defun make-fresh-array (size &optional (el nil) (type t) (simplep nil))
  (let ((res (make-array size
                         :fill-pointer (if (not simplep) size nil)
                         :adjustable (if (not simplep) t nil)
                         :initial-element el
                         :element-type type)))
    (map-into res #'(lambda (a) (setf a (cond
                                          ((functionp el)
                                           (funcall el))
                                          ((arrayp el)
                                           (a:copy-array el))
                                          ((listp el)
                                           (copy-list el))
                                          (t
                                           el))))
              res)))

(defun read-into-array (stream size &key (offset nil))
  (when offset
    (file-position stream offset))
  (let* ((bytes (make-array-frame size 0 '(unsigned-byte 8) t)))
    (read-sequence bytes stream)
    bytes))

(defun slurp-stream-into-array (stream)
  "Read all the octent from stream ad returns them as array"
  (a:read-stream-content-into-byte-vector stream))

(defun file-exists-p (f)
  (uiop:file-exists-p f))

(defun split-path-elements (path)
  (let ((splitted (cl-ppcre:split *directory-sep-regexp* path)))
    (substitute *directory-sep* "" splitted :test #'string=)))

(defun path-last-element (path)
  (let ((elements (split-path-elements path)))
    (and elements
         (a:last-elt elements))))

(defun directory-exists-p (d)
  (uiop:directory-exists-p d))

(defun subdirectories (parent)
  (uiop:subdirectories parent))

(defun make-directory (path)
  (if (not (cl-ppcre:scan (concatenate 'string *directory-sep* "$") path))
      (make-directory (concatenate 'string path *directory-sep*))
      (ensure-directories-exist path)))

(defun create-file (file)
  "create file and parent dir, if necessary"
  (let ((path-splitted (split-path-elements file)))
    (when (and path-splitted
               (> (length path-splitted) 1))
      (do* ((path-rest (subseq path-splitted 0 (1- (length path-splitted))) (rest path-rest))
            (path-so-far "" (if (and path-rest
                                     (not (string= "" (a:first-elt path-rest))))
                                (concatenate 'string
                                             path-so-far
                                             *directory-sep*
                                             (a:first-elt path-rest)
                                             *directory-sep*)
                                path-so-far)))
           ((null path-rest))
        (when (not (directory-exists-p path-so-far))
          (make-directory path-so-far)))
      (with-open-file (stream file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)))))

(defun namestring->pathname (path-as-string)
  (uiop:parse-native-namestring path-as-string))

(defun safe-subseq (sequence start &optional (end nil))
  "return the whole sequence if end is beyond the length of the sequence"
  (subseq sequence
          start
          (and end (min end (length sequence)))))

(defun vector-empty-p (v)
  #.nodgui.config:default-optimization
  (declare (vector v))
  (= (length v) 0))

(a:define-constant +png-magic-number+ #(#x89 #x50 #x4E #x47)             :test #'equalp)

(a:define-constant +gif89-magic-number+ #(#x47 #x49 #x46 #x38 #x39 #x61) :test #'equalp)

(a:define-constant +gif87-magic-number+ #(#x47 #x49 #x46 #x38 #x37 #x61) :test #'equalp)

(a:define-constant +jpeg-magic-number+ #(#xFF #xD8)                      :test #'equalp)

(defun check-magic-number (data &rest magic-numbers)
  (let* ((size-magic-number (length (first magic-numbers))))
    (loop for magic-number in magic-numbers do
         (when (every #'= (safe-subseq data 0 size-magic-number) magic-number)
           (return-from check-magic-number t)))
    nil))

(defmacro gen-check-magic-number (name &rest magics)
  (a:with-gensyms (stream data-from-file magic)
    (let ((predicate-name (format-fn-symbol t "~ap" name))
          (file-test-name (format-fn-symbol t "file-~a-p" name)))
      `(progn
         (defun ,predicate-name (data)
           (check-magic-number data ,@magics))
         (defun ,file-test-name (file-path)
           (ignore-errors
            (with-open-file (,stream
                             file-path
                             :direction :input
                             :element-type '(unsigned-byte 8))
              (loop for ,magic in (list ,@magics) do
                (let ((,data-from-file (make-array (length ,magic))))
                  (file-position ,stream :start)
                  (read-sequence ,data-from-file ,stream)
                  (when (,predicate-name ,data-from-file)
                    (return-from ,file-test-name t))))
              nil)))))))

(gen-check-magic-number png +png-magic-number+)

(gen-check-magic-number gif
                        +gif87-magic-number+
                        +gif89-magic-number+)

(gen-check-magic-number jpg +jpeg-magic-number+)

;;; colors

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun rgb-struct->tk-color (rgb-struct)
    (flet ((->byte (a)
             (round (* 255 a))))
      (format nil "#~2,'0X~2,'0X~2,'0X"
              (->byte (cl-colors2:rgb-red   rgb-struct))
              (->byte (cl-colors2:rgb-green rgb-struct))
              (->byte (cl-colors2:rgb-blue  rgb-struct)))))

  (declaim (inline rgb->tk))

  (defun rgb->tk (rgb-struct)
    (rgb-struct->tk-color rgb-struct))

  (defun read-color-macro (stream char ign)
    (declare (ignore char ign))
    (let ((raw (do* ((r   (read-char stream nil nil) (read-char stream nil nil))
                     (res '()))
                    ((char= r #\%) (coerce (reverse res) 'string))
                 (push r res))))
      (rgb-struct->tk-color (symbol-value (format-fn-symbol :cl-colors2 "+~a+" raw)))))

  (named-readtables:defreadtable nodgui-color-syntax
    (:fuse :standard)
    (:dispatch-macro-char #\# #\% #'read-color-macro)))

;;; numbers

(defgeneric make-tk-color (object &key colors-list))

(defmethod make-tk-color ((object cl-colors2:rgb) &key (colors-list nil))
  (declare (ignore colors-list))
  (rgb-struct->tk-color object))

(defmethod make-tk-color ((object string) &key (colors-list nil))
  (rgb-struct->tk-color (cl-colors2:as-rgb object :errorp t :colors-list colors-list)))

(defmethod make-tk-color ((object symbol) &key (colors-list nil))
  (rgb-struct->tk-color (cl-colors2:as-rgb object :errorp t :colors-list colors-list)))

(defun safe-parse-number (number &key (fix-fn #'(lambda (e) (declare (ignore e)) nil)))
  (handler-bind ((parse-error
                  #'(lambda (e)
                      (return-from safe-parse-number (funcall fix-fn e))))
                 (parse-number:invalid-number
                  #'(lambda (e)
                      (return-from safe-parse-number (funcall fix-fn e)))))
    (if (or (not (stringp number))
            (string= number "-"))
        nil
        (parse-number:parse-number number))))

(defparameter *default-epsilon* 1e-7)

(defmacro with-epsilon ((epsilon) &body body)
  `(let ((*default-epsilon* ,epsilon))
     ,@body))

(defun add-epsilon-rel (v &optional (epsilon *default-epsilon*))
  (+ v (* epsilon v)))

(defun epsilon<= (a b &optional (epsilon *default-epsilon*))
  (or (<= a b)
      (epsilon= a b epsilon)))

(defun epsilon>= (a b &optional (epsilon *default-epsilon*))
  (or (>= a b)
      (epsilon= a b epsilon)))

(defun epsilon= (a b &optional (epsilon *default-epsilon*))
  (and (<= (- b epsilon) a (+ b epsilon))))

;; binary parsing

(defmacro define-offset-size (package prefix &rest name-offset-size)
   `(progn
      ,@(loop for i in name-offset-size collect
             `(progn
                (a:define-constant
                    ,(a:format-symbol package "~@:(+~a-~a-offset+~)" prefix (first i))
                    ,(second i) :test #'=)
                ,(when (= (length i) 3)
                       `(a:define-constant
                            ,(a:format-symbol package "~@:(+~a-~a-size+~)" prefix
                                                       (first i))
                            ,(third i) :test #'=))))))

(defmacro define-parse-header-chunk ((name offset size object &optional (slot name)))
  (a:with-gensyms (bytes)
    `(progn
       (defgeneric ,(a:format-symbol t "PARSE-~:@(~a~)" name) (,object stream))
       (defmethod ,(a:format-symbol t "PARSE-~:@(~a~)" name) ((object ,object) stream)
         (file-position stream ,offset)
         (let* ((,bytes (make-fresh-list ,size)))
           (read-sequence ,bytes stream)
           ,(when (not (null slot))
                  `(setf (,slot object) ,bytes))
           (values ,bytes object))))))

(defun byte->int (bytes)
  (let ((res #x0000000000000000)
        (ct  0))
    (map nil #'(lambda (a)
                 (setf res (boole boole-ior
                                  (ash a ct)
                                  res))
                 (incf ct 8))
         bytes)
    res))

(defmacro gen-intn->bytes (bits)
  (let ((function-name (a:format-symbol t "~:@(int~a->bytes~)" bits)))
    `(defun ,function-name (val &optional (count 0) (res '()))
       (if (>= count ,(/ bits 8))
           (reverse res)
           (,function-name (ash val -8)
                           (1+ count)
                           (push (boole boole-and val #x00ff)
                                 res))))))

(gen-intn->bytes 16)

(gen-intn->bytes 32)

(defgeneric round-all (object &key rounding-function))

(defmethod round-all ((object list) &key (rounding-function #'round))
  (mapcar #'(lambda (n) (funcall rounding-function n)) object))

(defmethod round-all ((object number) &key (rounding-function #'round))
  (funcall rounding-function object))

(defmethod round-all ((object vector) &key (rounding-function #'round))
  (map (type-of object) #'(lambda (n) (funcall rounding-function n)) object))

(declaim (inline ->f))

(defun ->f (a)
  (coerce a 'single-float))

(defun rad->deg (rad)
  (/ (* rad 360.0) (* 2 pi)))

(defun deg->rad (deg)
  (/ (* deg 2 pi) 360.0))

(a:define-constant +valid-tcl-truth-values+ '("yes" "true") :test #'equalp)

(defun lisp-bool->tcl (val)
  (if val 1 0))

(defgeneric tcl-bool->lisp (val))

(defmethod tcl-bool->lisp ((val integer))
  (/= val 0))

(defmethod tcl-bool->lisp ((val string))
  (find val +valid-tcl-truth-values+ :test #'string=))

(defun tcl-error->boolean (val)
  (= val 0))

(defmacro gen-time-access (name pos)
  `(defun ,(format-fn-symbol t "time-~a-of" name) (time-list)
     (elt time-list ,pos)))

(defmacro gen-all-time-access (&rest name-pos)
  `(progn
     ,@(loop for i in name-pos collect
            `(gen-time-access ,(car i) ,(cdr i)))))

(gen-all-time-access (second     . 0)
                     (minutes    . 1)
                     (hour       . 2)
                     (date       . 3)
                     (month      . 4)
                     (year       . 5)
                     (day        . 6)
                     (daylight-p . 7)
                     (zone       . 8))

(defmacro try-unicode (char-name fallback)
  #+(or :sb-unicode :unicode :utf-8)
  (or (cl-unicode:character-named char-name :try-lisp-names-p t)
      fallback)
  #-(or :sb-unicode :unicode :utf-8) fallback)

;; some useful unicode symbols

(defun right-arrow ()
  (string (try-unicode "RIGHTWARDS_BLACK_ARROW" ">")))

(defun left-arrow ()
  (string (try-unicode "LEFTWARDS_BLACK_ARROW" "<")))

(defun double-right-arrow ()
  (string (try-unicode "U2BEE" ">>")))

(defun double-left-arrow ()
  (string (try-unicode "U2BEC" "<<")))

(defun up-arrow ()
  (string (try-unicode "UPWARDS_ARROW" "Up")))

(defun down-arrow ()
  (string (try-unicode "DOWNWARDS_ARROW" "Down")))

(defun big-dot ()
  (string (try-unicode "BLACK_LARGE_CIRCLE" ".")))

(defun bullet ()
  (string (try-unicode "BULLET" "*")))

(defparameter *thread-default-special-bindings* bt2:*default-special-bindings*)

(defmacro definline (name arg &rest body)
  (let* ((function-name (a:format-symbol t "~:@(~a~)" name)))
    `(progn
       #+optimize-nodgui (declaim (inline ,function-name))
       (defun ,function-name (,@arg) ,@body))))

(definline make-thread (function &key (name nil) (initial-bindings *thread-default-special-bindings*))
  (bt2:make-thread function
                   :name name
                   :initial-bindings (append initial-bindings
                                             (list (cons '*read-default-float-format*
                                                         ''single-float)))))

(definline make-lock (&optional name)
  (bt2:make-lock :name name))

(defmacro with-lock-held ((lock) &body body)
  `(bt2:with-lock-held (,lock)
     ,@body))

(definline make-condition-variable (&key (name nil))
  (bt2:make-condition-variable :name name))

(definline condition-wait (condition-variable lock &key (timeout nil))
  (bt2:condition-wait condition-variable lock :timeout timeout))

(definline condition-notify (condition-variable)
  (bt2:condition-notify condition-variable))

(definline join-thread (thread)
  (bt2:join-thread thread))

(definline destroy-thread (thread)
  (bt2:destroy-thread thread))

(definline threadp (maybe-thread)
  (bt2:threadp maybe-thread))

(definline thread-alive-p (thread)
  (bt2:thread-alive-p thread))

(defmacro define-compiler-macro* (name &body args)
  (a:with-gensyms (low-level-function-name)
    `(progn
       (defun ,low-level-function-name (,@args)
         (declare (notinline ,name))
         (,name ,@args))
       (define-compiler-macro ,name (&whole form ,@args)
         (let ((low-funname ',low-level-function-name))
           (if (every #'constantp (list ,@args))
               (funcall (symbol-function low-funname) ,@args)
               (progn
                 form)))))))

(cffi:defcfun (%memcpy "memcpy")
    :pointer
  (destination   :pointer)
  (source        :pointer)
  (size          :unsigned-int))

(defun copy-ffi-vector (source-pointer destination-pointer octets-size)
  (%memcpy destination-pointer source-pointer octets-size))
