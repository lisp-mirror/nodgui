;; This software is Copyright (c) 2003-2010  Peter Herth <herth@peter-herth.de>
;; Portions Copyright (c) 2005-2010 Thomas F. Burdick
;; Portions Copyright (c) 2006-2010 Cadence Design Systems
;; Portions Copyright (c) 2010 Daniel Herring
;; Portions Copyright (c) 2019 cage

;; The  authors  grant you  the  rights  to  distribute and  use  this
;; software as  governed by the  terms of  the Lisp Lesser  GNU Public
;; License  (http://opensource.franz.com/preamble.html), known  as the
;; LLGPL.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more details.

(in-package :nodgui.tklib.notify)

(named-readtables:in-readtable nodgui.tcl-emitter:nodgui-force-escape-syntax)

(define-constant +notify-library-name+ "notifywindow" :test #'string=)

(define-constant +bell-icon+
    (strcat "iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA7wDGACtqWfjDAA"
            "AACXBIWXMAAA7DAAAOwwHHb6hkAAABXklEQVRYw9WWPU4DQQyF7RUpo1UKGlqUioqWG+wd"
            "0uUA6RElF+AAdLlDbpCWigbECSiiUZpIkTJUg2bNeO3xzhB41f5pbH9+6xkEo54fr318v3"
            "z4QMs6jTG46plG2VnvtjceAKC9vE1XNF9jNQKn94Uv8Y2JQKg8iCOQS6KBMwsVSAEAwH2+"
            "sL130xXA4Q3a4+bH2s18PZ6AGDxcT7r4lcoLF9rgnNr9Uy8RBxCT8BLlKh4gJGwEot5nBS"
            "c++F6H84KagPTbWdWcE7+YgGRAZRI+ywOp3ufgDx5wk67nB84LQwR8wXZ4cRKGDFP4tQTC"
            "LAizgZBAslfwBCzBaWAtURyq3PL7UQqhDZEfkPWA1fWZ8kkCqeBjho+brjgv9HfLGsFTOy"
            "VHAulJp/TYpSTESVhj5g+RQOmUW1t//0wonY6pZnev+K8IjE/g6v7XE8CBJLC6Bzgv5Pa+"
            "RAuwRCFf0dKhJufiL2QAAAAASUVORK5CYII=")
  :test          #'string=
  :documentation "A bell icon in png format.")

(defun notify-window (message &optional (image (make-image +bell-icon+)))
  "Show a notification to the user. According to the documentation the
message's length should  be 40 characters but even their  demo shows a
longer text.   Also the documentation  says the image should  has size
equal to 16x16 pixel but their default is 20x20; I found that 32x32 is
fine too. :)"
  (require-tcl-package +notify-library-name+)
  (let ((*suppress-newline-for-tcl-statements*             t)
        (*add-space-after-emitted-unspecialized-element*   nil)
        (*accept-garbage-as-event-p*                       t))
    ;; the tcl  library has a  spurious 'puts' command that  print the
    ;; width  of the  window  this will  crash nodgui  so  i made  the
    ;; library intercepts  and discards  this value  with the  call of
    ;; 'process-event' below. Because an event should be a list (and a
    ;; number  is  not) also  we  need  to  set the  special  variable
    ;; *accept-garbage-as-event-p* to non nil to discard this event
    (format-wish (tclize `(senddata
                           ["::notifywindow::notifywindow" " "
                           { ,#[message ] }     " "
                           ,(empty-string-if-nil image
                                `([ image create photo -data ,#[(nodgui::data image) ]]])))))
    (process-events t)))
