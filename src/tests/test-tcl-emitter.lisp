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

(in-package :test-tcl-emitter)

(defsuite tcl-emitter-suite (all-suite))

(defun %join (&rest tokens)
  (join-with-strings tokens (string #\Newline)))

(deftest test-nil (tcl-emitter-suite)
  (let ((nil-in-lisp      (tcl-str (:lisp nil)))
        (nil-in-tcl-macro (tcl-str nil)))
    (assert-true (string= "" nil-in-lisp))
    (assert-true (string= "" nil-in-tcl-macro))))

(defmacro defcode-test ((name &body tcl-ast) &rest result-lines)
  (with-gensyms (code res)
    `(deftest ,name (tcl-emitter-suite)
       (let ((,code ,(if (eq (first (first tcl-ast)) 'defproc)
                         `(string ,@tcl-ast)
                         `(tcl-str ,@tcl-ast)))
             (,res  (%join   ,@result-lines)))
         (assert-true (string= ,res (trim ,code)))))))

(defcode-test (test-with-flush-server
               (:lisp
                (with-flush-server
                    (tcl
                      (global server)
                      (set res [ string cat <  #.+wish-to-lisp-data-reply+
                           \\+ \"+ [ escape $s ]+ \\+ \" > ])))))
    "global server"
  "set res [ string cat \"(\" \":data\" \\\"[ escape $s ]\\\"  \")\"]"
  "flush $server")

(defcode-test (test-send-datastring
               (defproc senddatastring (s)
                 (:lisp
                  (with-flush-server
                      (tcl
                        (global server)
                        (set res [ string cat <  #.+wish-to-lisp-data-reply+
                             \\+ \"+ [ escape $s ]+ \\+ \" > ]))))))
    "proc senddatastring {s "
  "} {"
  "    global server"
  "set res [ string cat \"(\" \":data\" \\\"[ escape $s ]\\\"  \")\"]"
  "flush $server"
  "}")

(defcode-test (test-for-list
               (:lisp
                (let ((j 5))
                  (for-list i (list j) (1+ i) (+ 2 i))))
               (8))
    "7" "8")

(defcode-test (test-send-data
               (:if ([ catch {+ ~a+ } ])
                    (puts \"+ < #.+wish-to-lisp-error-reply+ \\+
                          \"+ [escape $result]+ \\+ \" > ])
                    (senddatastring $result)))
    "if { [ catch {~a} ]"
  "} { "
  "puts \"\"(\" \":error\" \\\"[escape $result]\\\"  \")\"]"
  ""
  "} else { "
  "senddatastring $result"
  "}")

(defcode-test (test-require-ttk
               (:if ([catch {package require "Ttk" } err ])
                    ("tk_messageBox" \++ -icon error -type ok -message $err%)))
    "if { [catch {package require Ttk} err ]"
  "} { "
  "tk_messageBox -icon error -type ok -message \"$err\""
  ""
  "} else { "
  "}")

(defcode-test (test-debug-proc
                    (defproc debug ( msg )
                      (:lisp
                       (with-flush-server
                           (tcl
                             (global server)
                             (puts $server [ strcat < :debug \\+ \"+ [escape $msg ]+ \\+ \" > ]))))))
    "proc debug {msg "
  "} {"
  "    global server"
  "puts $server [ strcat \"(\" \":debug\" \\\"[escape $msg ]\\\"  \")\"]"
  "flush $server"
  "}")

(defcode-test (test-escape-proc
               (defproc escape (s)
                 (regsub -all {+ \\\\+ } $s {+ \\\\\\\\+ } s1)
                 (regsub -all {\"} $s1 {\\+ \"} s2)
                 (return $s2)))
  "proc escape {s "
  "} {"
  "    regsub -all {\\\\} $s {\\\\\\\\} s1"
  "    regsub -all {\"} $s1 {\\\"} s2"
  "    return $s2"
  "}")


(defcode-test (test-send-data-strings-proc
               (defproc senddatastrings (strings)
                 (:lisp
                  (with-flush-server
                      (tcl
                        (global server)
                        (puts $server [strcat < #.+wish-to-lisp-data-reply+ < ])
                        (foreach s $strings {)
                        (puts $server [strcat  \"+ \\+ \"+ [escape $s]+
                              \\+ \"+ \"  > > ] }))))))
    "proc senddatastrings {strings "
  "} {"
  "    global server"
  "puts $server [strcat \"(\" \":data\" \"(\" ]"
  "foreach s $strings {"
  "puts $server [strcat \"\\\"[escape $s]\\\"\"  \")\" \")\"] }"
  "flush $server"
  "}")

(defcode-test (test-to-keyword-proc
               (defproc to_keyword (s)
                 (:if ([string index $s 0] == \"+ -+ \")
                      (return \"+ \:+ [string range $s 1 [string length $s ]]+ \")
                      (return \"+ \:+ $s+ \"))))
    "proc to_keyword {s "
  "} {"
  "    if { [string index $s 0] == \"-\""
  "} { "
  "return \":[string range $s 1 [string length $s ]]\""
  ""
  "} else { "
  "return \":$s\""
  "}"
  "}")

(defcode-test (test-send-property-list-proc
               (defproc sendpropertylist (l)
                 (global server)
                 (set pos 0)
                 (set ll [llength $l])
                 (puts $server [strcat < #.+wish-to-lisp-data-reply+ < ])
                 (while {$pos .< \ $ll} {)
                 (puts $server \" [to_keyword [lindex $l $pos]] \")
                 (set pos [expr $pos .+ \ 1])
                 (puts $server \" [lindex $l $pos] \")
                 (set pos [expr $pos .+ \ 1])
                 (})
                 (puts $server [strcat > > ])))
    "proc sendpropertylist {l "
  "} {"
  "    global server"
  "    set pos 0"
  "    set ll [llength $l]"
  "    puts $server [strcat \"(\" \":data\" \"(\" ]"
  "    while {$pos < $ll} {"
  "    puts $server \" [to_keyword [lindex $l $pos]] \""
  "    set pos [expr $pos + 1]"
  "    puts $server \" [lindex $l $pos] \""
  "    set pos [expr $pos + 1]"
  "    }"
  "    puts $server [strcat  \")\" \")\"]"
  "}")
