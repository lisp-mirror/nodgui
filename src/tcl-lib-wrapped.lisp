(in-package :nodgui.tcl-lib-wrapped)

(defun match-path (text &key (root-directory nil) (join nil) (path nil) (type nil))
  (let ((chars (remove-duplicates (coerce (string-downcase (symbol-name type))
                                          'list)
                                  :test #'char=)))
    (assert (or (null type)
                (every (lambda (a) (member a
                                           '(#\b #\c #\d #\f #\/ #\p #\s #\r #\w #\x)
                                           :test #'char=))
                       chars))))
  (flet ((glob ()
           (with-read-data ()
             (let ((*suppress-newline-for-tcl-statements* t))
               (format-wish (tclize `((senddatastrings [ glob
                                        ,(empty-string-if-nil root-directory
                                           `(-directory ,root-directory)) " "
                                           ,(empty-string-if-nil join
                                              `(-join))                 " "
                                           ,(empty-string-if-nil path
                                              `(-path ,path))           " "
                                           ,(empty-string-if-nil type
                                              `(-type ,type))           " "
                                              -nocomplain                  " "
                                              --                           " "
                                              ,text
                                              ]))))))))
    (sort (glob) #'string<)))

(a:define-constant +tcl-csv-libname+ "csv" :test #'string=)

(defun parse-csv-line (line &key
                              (separator ",")
                              (quote-char "\""))
  (with-read-data ()
    (let ((*suppress-newline-for-tcl-statements* t))
      (format-wish (tclize `(senddatastrings [ "csv::split "
                                             ,line " "
                                             ,separator " "
                                             ,quote-char ]))))))

(defun csv-stream (stream &key
                            (separator ",")
                            (quote-char "\""))
  "Needs tcllib. Returns a closure that, when invoked with no argument, returns a list corresponding to a row of the table represented by the CSV data (or nil atfter the last row has been parsed), the data are split using `separator' and each field cn be quoted using `quote-char'."
  (require-tcl-package +tcl-csv-libname+)
  (lambda ()
    (a:when-let ((line (read-line stream nil nil)))
      (parse-csv-line line :separator separator :quote-char quote-char))))

(a:define-constant +tcl-zip-libname+ "zipfile::mkzip" :test #'string=)

(defun make-zip-file (zip-filepath glob-paths)
  "Creates a zip file in `zip-filepaths' adding a list of paths contained in `glob-paths'.
Each element of `glob-paths' uses globs e.g '(\"*.zip\" \"foo.*\")"
  (require-tcl-package +tcl-zip-libname+)
  (let ((*suppress-newline-for-tcl-statements* t))
    (format-wish (tclize `("zipfile::mkzip::mkzip "
                           {+ ,zip-filepath } " "
                           (-- ,(make-bypass-escape :data (format nil "~{{~a} ~}"
                                                                  glob-paths))))))))

(a:define-constant +tcl-nettool-libname+ "nettool" :test #'string=)

(defun cpu-info ()
  "Returns an alist of CPU information field (on GNU/Linux) are:
- :memory
- :vendor
- :family
- :model
- :brand
- :steppin
- :speed
- :features
- :cpus"
  (require-tcl-package +tcl-nettool-libname+)
  (let ((raw-data (with-read-data ()
                    (let ((*suppress-newline-for-tcl-statements* t))
                      (format-wish (tclize `(senddatastrings [ "nettool::cpuinfo"])))))))
    (loop for (key value) on raw-data by 'cddr
          collect
          (let ((actual-value (nodgui.utils:safe-parse-number value
                                                              :fix-fn
                                                              (lambda (e)
                                                                (declare (ignore e))
                                                                value))))
          (cons (a:make-keyword (string-upcase key))
                actual-value)))))

(defun mac-addresses ()
  "Returns an alist of MAC addresses for this machines, the primary is listed first"
  (require-tcl-package +tcl-nettool-libname+)
  (let ((raw-data (with-read-data ()
                    (format-wish (tclize `(senddatastrings [ "nettool::mac_list"]))))))
    (loop for mac in raw-data
          collect
          (let ((splitted (cl-ppcre:split ":" mac)))
            (loop for octet in splitted collect (parse-integer octet :radix 16))))))
