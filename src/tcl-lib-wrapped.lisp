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
                           \"+ ,zip-filepath \"
                           (-- ,@(wrap-with-double-quotes glob-paths)))))))

(a:define-constant +tcl-zip-decode-libname+ "zipfile::decode" :test #'string=)

(defun zip-file-p (zip-filepath)
  "Creates a zip file in `zip-filepaths' adding a list of paths contained in `glob-paths'.
Each element of `glob-paths' uses globs e.g '(\"*.zip\" \"foo.*\")"
  (when (u:file-exists-p zip-filepath)
    (require-tcl-package +tcl-zip-decode-libname+)
    (u:tcl-bool->lisp (with-read-data ()
                        (let ((*suppress-newline-for-tcl-statements* t))
                          (format-wish (tclize `(senddata [ "zipfile::decode::iszip "
                                                          \"+ ,zip-filepath \" ]))))))))

(defun zip-file-list-contents (zip-filepath)
  "Creates a zip file in `zip-filepaths' adding a list of paths contained in `glob-paths'.
Each element of `glob-paths' uses globs e.g '(\"*.zip\" \"foo.*\")"
  (when (zip-file-p zip-filepath)
    (with-read-data ()
      (let ((*suppress-newline-for-tcl-statements* t))
        (format-wish (tclize `(senddatastrings [ "zipfile::decode::content "
                                               \"+ ,zip-filepath \" ])))))))

(defun unzip-file (zip-filepath destination-path)
  "Decompress a zip file in `zip-filepaths' on `destination-path'."
  (when (zip-file-p zip-filepath)
    (with-read-data ()
      (let ((*suppress-newline-for-tcl-statements* t))
        (format-wish (tclize `(senddatastrings [ "zipfile::decode::unzipfile "
                                               \"+ ,zip-filepath \"
                                               \"+ ,destination-path \"
                                               ])))))))

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

(defun %collect-split-addresses (command separator radix)
  (let ((raw-data (with-read-data ()
                    (format-wish (tclize `(senddatastrings [ ,command ]))))))
    (loop for mac in raw-data
          collect
          (let ((splitted (cl-ppcre:split separator mac)))
            (loop for octet in splitted collect (parse-integer octet :radix radix))))))

(defun mac-addresses ()
  "Returns a list of MAC addresses for this machines, the primary is listed first"
  (require-tcl-package +tcl-nettool-libname+)
  (%collect-split-addresses "nettool::mac_list" ":" 16))

(defun ip-addresses ()
  "Returns a list of IP addresses for this machines."
  (require-tcl-package +tcl-nettool-libname+)
  (%collect-split-addresses "nettool::ip_list" "\\." 10))

(a:define-constant +tcl-units-libname+ "units" :test #'string=)

(defun convert-units (from to)
  "Convert quantity to differents measure units e.g.:

(convert-units \"1 mm\" \"meter\")

Please see:

 https://core.tcl-lang.org/tcllib/doc/trunk/embedded/md/tcllib/files/modules/units/units.md

For more documentation"
  (require-tcl-package +tcl-units-libname+)
  (with-read-data ()
    (format-wish (tclize `(senddata [ "units::convert " \"+ ,from \" \"+ ,to \" ])))))

(defun reduce-unit (unit-string)
  "Reduce `unit-string' to their atomic components e.g.:

(reduce-units \"Joule\"); => \"1000.0 gram meter meter / second second\"

Please see:

 https://core.tcl-lang.org/tcllib/doc/trunk/embedded/md/tcllib/files/modules/units/units.md

For more documentation."
  (require-tcl-package +tcl-units-libname+)
  (with-read-data ()
    (format-wish (tclize `(senddatastring [ "units::reduce " \"+ ,unit-string \"])))))

(defun new-unit (unit equivalent-to)
  " Create a nes unit equivalent to `equivalent-to' e.g.
  (with-nodgui ()
    (new-unit \"furlong\" \"220 yards\")
    (convert-units \"10 furlong\" \"yards\")) ; => 2200.0

Please see:

 https://core.tcl-lang.org/tcllib/doc/trunk/embedded/md/tcllib/files/modules/units/units.md

For more documentation."
  (require-tcl-package +tcl-units-libname+)
  (with-read-data ()
    (format-wish (tclize `(senddatastring [ "units::new " \"+ ,unit \" \"+ ,equivalent-to \"])))))

(a:define-constant +tcl-fileutil-libname+ "fileutil" :test #'string=)

(defun file-writable-p (path)
  "Return true if file in `path' is writable."
  (require-tcl-package +tcl-fileutil-libname+)
  (when (u:file-exists-p path)
    (u:tcl-bool->lisp (with-read-data ()
                        (let ((*suppress-newline-for-tcl-statements* t))
                          (format-wish (tclize `(senddata [ "fileutil::test "
                                                          \"+ ,path \"
                                                          "w"
                                                         ]))))))))

(defun replace-in-file (path start size data)
  "Remove `size' octets from file pointed by `path' starting at position: `start', replacing with `data', return `t' on success."
  (require-tcl-package +tcl-fileutil-libname+)
  (when (and (u:file-exists-p path)
             (u:file-exists-p path))
    (format-wish (tclize `("::fileutil::replaceInFile "
                           \"+ ,path \"
                           \"+ ,start \"
                           \"+ ,size \"
                           \"+ ,data \")))
    t))
