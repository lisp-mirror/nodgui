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

(in-package :nodgui.tcl-glue-code)

(define-constant +wish-init-code-raw+
    (strcat

     (tcl-str (:if ([catch {package require "Ttk" } err ])
                   ("tk_messageBox" \++ -icon error -type ok -message $err%)))

     (defproc strcat (args)
       (set res .%)
        (foreach i $args { set res [ string cat $res $i ] })
        (return $res))

     (defproc debug ( msg )
       (:lisp
         (with-flush-server
             (tcl
               (global server)
               (puts $server [ strcat < :debug
                               \"+ \\+ \"+ [escape $msg+ ]+ \\+ \"+ \"+
                               > ])))))

     (defproc escape (s)
       (regsub -all {+ \\\\+ } $s {+ \\\\\\\\+ } s1)
       (regsub -all {\"} $s1 {\\+ \"} s2)
       (return $s2))

     (defproc senddata (s)
       (:lisp
        (with-flush-server
            (tcl
              (global server)
              (puts $server [ strcat < #.+wish-to-lisp-data-reply+ \ %
                    \"+ [ escape $s ]+ \" > ])))))

     (defproc senddatastring (s)
       (:lisp
        (with-flush-server
            (tcl
              (global server)
              (puts $server [strcat < #.+wish-to-lisp-data-reply+
                    \"+ \\+ \"+ [escape $s]+ \\+ \"+ \" > ])))))

     (defproc senddatastrings (strings)
       (:lisp
        (with-flush-server
            (tcl
              (global server)
              (puts $server [strcat < #.+wish-to-lisp-data-reply+ < ])
              (foreach s $strings {)
              (puts $server [strcat  \"+ \\+ \"+ [escape $s]+ \\+ \"+ \" ] })
              (puts $server [strcat > > ])))))

     (defproc to_keyword (s)
       (:if ([string index $s 0] == \"+ -+ \")
            (return \"+ \:+ [string range $s 1 [string length $s ]]+ \")
            (return \"+ \:+ $s+ \")))

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
       (puts $server [strcat > > ]))

     (defproc searchall (widget pattern)
       (set l       [string length $pattern])
       (set results [$widget search $pattern 1.0])
       (set previous 0)
       (while {$results .> \  $previous} {)
       ($widget tag add sel $results $results+${l}chars)
       (set previous $results)
       (set result [$widget search $pattern $results+${l}chars] }))

     (defproc searchnext (widget pattern)
       (set l [string length $pattern])
       (set result [$widget search $pattern insert])
       (:if ($result .> \ 0)
            (:group
             ($widget tag remove sel 1.0 end)
             ($widget tag add sel $result $result+${l}chars)
             ($widget mark set insert $result+${l}chars)
             ($widget see insert))))

     (defproc resetscroll (c)
       ($c configure -scrollregion [$c bbox all]))

  (defproc movetostart (sb)
    (set range [$sb get])
    ($sb set 0 [expr [lindex $range 1] - [lindex $range 0]]))

  (defproc sendevent (s x y
                        charcode keycode char
                        width height
                        root_x root_y
                        mouse_button
                        ({+ other {+ }+ }))
     (:lisp
     (with-flush-server
         (tcl
           (global server)
           (puts $server [strcat < #.+wish-to-lisp-event-reply+ \\+ \"+ $s+ \\+ \"
                                   \ % $x \ % $y \ %  $charcode
                                   \ % $keycode \ % \\+ \"+ $char+ \\+ \" \ %
                                   $width \ % $height \ %
                                   $root_x \ % $root_y \ % $mouse_button \ %
                                   \\+ \" $other+ \\+ \" \ %
                                 > ])))))

  (defproc callback (s)
    (:lisp
     (with-flush-server
         (tcl
           (global server)
           (puts $server [strcat <  #.+wish-to-lisp-callback-reply+ \\+ \"+ $s+ \\+ \"+
                                 > ])))))

  (defproc callbackval (s val)
    (global server)
    (puts $server [strcat <  #.+wish-to-lisp-callback-reply+ \\+ \"+ $s+ \\+ \"  $val > ]))

  (defproc callbackstring (s val)
    (global server)
    (puts $server [strcat <  #.+wish-to-lisp-callback-reply+
                   \\+ \"+ $s+ \\+ \" \\+ \"+ [escape $val]+ \\+ \" > ]))

  (defproc keepalive ()
    (:lisp
     (with-flush-server
         (tcl
           (global server)
           (puts $server [strcat <  #.+wish-to-lisp-keepalive-reply+
                 \\+ \"+ [clock format [clock seconds]
                 -format \"+ %d/%m/%y "%T" \"+ ]+ \\+ \"+ \"  > ] ]))))))
  :test #'string=)

(define-constant +tcl-init-code+
    (strcat

     (tcl-str
       (set buffer {})
       (set server stdout)
       (set tclside_nodguidebug ~+ \:+ [0~+ \;1~])
       (package require "Tk")
       (wm protocol ". WM_DELETE_WINDOW" \ exit)

       (:if ($tclside_nodguidebug)
            (:group
             (toplevel ".nodgui")
             (wm title ".nodgui" \  \"+ "Debug output" \")
             (text ".nodgui.debug" \ -height 20)
             (pack ".nodgui.debug" \ -side left -expand 1 -fill both)
             (scrollbar ".nodgui.vs" \ -orient vertical -command {".nodgui.debug" \ yview})
             (".nodgui.debug" \ configure -yscrollcommand {".nodgui.vs" \ set})
             (pack ".nodgui.vs" \ -side right -fill y))))

     (defproc nodguidebug (text)
       (global tclside_nodguidebug)
       (:if ($tclside_nodguidebug)
            (:group
             (".nodgui.debug" \ insert end \"+ $text+ \\n+ \")
             (".nodgui.debug" \ see end))))

     (defproc getcount (s)
       (:if ([regexp {^\\s*\(\\d+\) } $s match num])
            (return $num)))

     (defproc getcount (s)
       (:if ([regexp {^\\s*\(\\d+\) } $s match])
            (return [string range $s [string length $match] end])))

     (defproc process_buffer ()
       (global buffer)
       (global server)
       (set cmd $buffer)
       (set buffer {})
       (set errs {})
       (:if ([catch $cmd] .> \ 0)
            (:group
             (:lisp
              (with-flush-server
                  (tcl
                    (set errs [regsub -all {+ "[^[:alnum:][[:punct:] ]+" }
                         "$::errorInfo" " " \~%% ])
                    (puts stderr $errs)
                    (puts $server [strcat < #.+wish-to-lisp-error-reply+
                         \"+ \\+ \"+ [escape $errs]+ \\+ \"+ \"  > ])))))))

     (defproc bt (txt)
       (global buffer)
       (append buffer $txt))

     (tcl-str
       (fconfigure stdin -encoding utf-8 -translation ~a)
       (fconfigure stdout -encoding utf-8)
       (\#fileevent stdin readable sread)))

  :test #'string=)

(define-constant +tcl-send-data-code+
    (tcl-str (:if ([ catch {+ ~a+ } ])
                  (puts \"+ < #.+wish-to-lisp-error-reply+ \\+
                        \"+ [escape $result]+ \\+ \" > ])
                  (senddatastring $result)))
  :test #'string=)

(defun wish-init-code ()
  +wish-init-code-raw+)

(defun tcl-init-code ()
  +tcl-init-code+)

(defun tcl-send-data-code ()
  +tcl-send-data-code+)
