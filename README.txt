1 NODGUI
════════

  /"No Drama GUI"/

  Common Lisp bindings for the Tk GUI toolkit.


2 Dependency
════════════

2.1 Programs
────────────

2.1.1 TCL/TK interpreter (version >= 8.6)
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  [https://www.tcl.tk]


2.2 Lisp libraries
──────────────────

  • alexandria;
  • cl-ppcre-unicode;
  • cl-lex;
  • cl-yacc;
  • clunit2;
  • cl-syntax.

  All of the above libraries are available on quicklisp.


3 FAQ
═════


  Where is the documentation?
               nodgui is just the results of a code refactoring of LTK
        with some syntax sugar on top.  This means that the [original
        documentation] can be *almost* used as a good reference also for
        this library; the chapters that are still valid are listed
        below:

        • [Introduction] ;
        • [Tutorial] ;
        • [Event handling] ;
        • [Screen functions] ;
        • [Window manager functions] ;
        • [Under the hood] .

        Please note that all the documentation about [the widgets] *is
        outdated* instead.

  Where can i found documentation about a widget?
               It is actually very simple: let's suppose you want to use
        a spinbox widget in your program and you have no idea how to do,
        the idea to solve this problem is like:

        1. open the file `src/spinbox.lisp' an look at the top of this
           file:

        ┌────
        │ (defargs spinbox ()
        │ cursor
        │ state
        │ style
        │ takefocus
        │ validate
        │ validatecommand
        │ xscrollcommand
        │ command
        │ format
        │ from
        │ increment
        │ to
        │ values
        │ wrap)
        └────

        the symbols listed are all the options for the spinbox TK
        command (in TCL language everything is a command) or the
        configuration variable that you can pass as initarg for `(make
        instance 'spinbox ...)' and, sometimes, changes after object
        instancing via `configure' ([see the general documentation])

        1. for the meaning of this symbols refers to the
        [original TK documentation]

        As a general rule choose the link to the command with its name
        prefixed with "ttk::" (if exists).

        In our example point to:

        [https://www.tcl.tk/man/tcl8.6/TkCmd/ttk_spinbox.htm]

        and *not*

        [https://www.tcl.tk/man/tcl8.6/TkCmd/spinbox.htm]

        You can easly understand what all the options above are for.

  What are the differences between nodgui and LTK?
        1. A reader macro for events; i.e.  `#$<Alt-q>$' instead of
           `"<Alt-q>"' (a string), the macro will parse and check for
           trivial errors in the event definition syntax at compile
           time.

           /Please note that a string is still accepted as event
           specifier./
        2. A DSL for TCL code that allow to mix TCL code and lisp
           expression, check for example the following code to create a
           bitmap:

           ┌────
           │ (tclize `([list
           │           ,(loop for r from 0 below h collect
           │               (tclize `([list ,(loop
           │   ...
           └────

           The `tclize' macro will transforms the backquoted forms in
           tcl code, the unquoted forms will be evaluated before the
           translation occurs.

        3. The function [`postscript'] returns a postscript file as
           string instead of write a file on disk;

        4. Compatible only with TK 8.6;

        5. A general refactoring.

  Where can i find more code examples?
               check the file `src/demo-tests.lisp'

  Can I contribute to this project?
               Yes, of course! Please open an issue or a pull request on
        the [web repository], if you do not feel comfortable with coding
        documentation improvements are very welcome too! :)

        Also i would appreciate [testing] if the library works on
        different environment than mine (debian GNU/Linux with SBCL).


[original documentation] http://www.peter-herth.de/ltk/ltkdoc/

[Introduction] http://www.peter-herth.de/ltk/ltkdoc/node2.html

[Tutorial] http://www.peter-herth.de/ltk/ltkdoc/node4.html

[Event handling] http://www.peter-herth.de/ltk/ltkdoc/node14.html

[Screen functions] http://www.peter-herth.de/ltk/ltkdoc/node41.html

[Window manager functions]
http://www.peter-herth.de/ltk/ltkdoc/node42.html

[Under the hood] http://www.peter-herth.de/ltk/ltkdoc/node43.html

[the widgets] http://www.peter-herth.de/ltk/ltkdoc/node17.html

[see the general documentation] See figure (3)

[original TK documentation]
https://www.tcl.tk/man/tcl8.6/TkCmd/contents.htm

[`postscript'] https://www.tcl.tk/man/tcl8.6/TkCmd/canvas.htm#M61

[web repository] https://notabug.org/cage/nodgui

[testing] See section 4.1


4 Status
════════

  All tk commands as of version 8.4 with support information. "-" means
  not supported by purpose (look comment), "x" means supported, though
  some options may not be supported.

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   command                 supported  comment
  ────────────────────────────────────────────────────────────────────────────────
  ────────────────────────────────────────────────────────────────────────────────
   `bell'                  x
  ────────────────────────────────────────────────────────────────────────────────
   `bind'                  x
  ────────────────────────────────────────────────────────────────────────────────
   `bindtags'              modify     modify the tag list of a widget that
                                      describes which events it gets
  ────────────────────────────────────────────────────────────────────────────────
   `bitmap'                -          see image
  ────────────────────────────────────────────────────────────────────────────────
   `button'                x
  ────────────────────────────────────────────────────────────────────────────────
   `canvas'                x
  ────────────────────────────────────────────────────────────────────────────────
   `checkbutton'           x
  ────────────────────────────────────────────────────────────────────────────────
   `clipboard'             x          (canvas get missing… tricky…)
  ────────────────────────────────────────────────────────────────────────────────
   `colors'                -          constants only
  ────────────────────────────────────────────────────────────────────────────────
   `console'               -          only on some platforms
  ────────────────────────────────────────────────────────────────────────────────
   `cursors'               x
  ────────────────────────────────────────────────────────────────────────────────
   `destroy'               x
  ────────────────────────────────────────────────────────────────────────────────
   `entry'                 x
  ────────────────────────────────────────────────────────────────────────────────
   `event'                 create     create and manage virtual events
  ────────────────────────────────────────────────────────────────────────────────
   `focus'                 x          focus management functions
  ────────────────────────────────────────────────────────────────────────────────
   `font'
  ────────────────────────────────────────────────────────────────────────────────
   `frame'                 x
  ────────────────────────────────────────────────────────────────────────────────
   `grab'
  ────────────────────────────────────────────────────────────────────────────────
   `grid'                  x
  ────────────────────────────────────────────────────────────────────────────────
   `image'                 x
  ────────────────────────────────────────────────────────────────────────────────
   `keysyms'               -          constants only
  ────────────────────────────────────────────────────────────────────────────────
   `label'                 x
  ────────────────────────────────────────────────────────────────────────────────
   `labelframe'            x
  ────────────────────────────────────────────────────────────────────────────────
   `listbox'               x
  ────────────────────────────────────────────────────────────────────────────────
   `loadTk'                -
  ────────────────────────────────────────────────────────────────────────────────
   `lower'                 x
  ────────────────────────────────────────────────────────────────────────────────
   `menu'                  x
  ────────────────────────────────────────────────────────────────────────────────
   `menubutton'            x
  ────────────────────────────────────────────────────────────────────────────────
   `message'               x
  ────────────────────────────────────────────────────────────────────────────────
   `option'                -
  ────────────────────────────────────────────────────────────────────────────────
   `options'               -          only helpfile
  ────────────────────────────────────────────────────────────────────────────────
   `pack'                  x
  ────────────────────────────────────────────────────────────────────────────────
   `panedwindow'           x
  ────────────────────────────────────────────────────────────────────────────────
   `photo'                 x          support for PNG, GIF and raw RGB(A) format.
  ────────────────────────────────────────────────────────────────────────────────
   `place'                 x          geometry manager using coordinates
  ────────────────────────────────────────────────────────────────────────────────
   `radiobutton'           x
  ────────────────────────────────────────────────────────────────────────────────
   `raise'                 x
  ────────────────────────────────────────────────────────────────────────────────
   `scale'                 x
  ────────────────────────────────────────────────────────────────────────────────
   `scrollbar'             x
  ────────────────────────────────────────────────────────────────────────────────
   `selection'
  ────────────────────────────────────────────────────────────────────────────────
   `send'
  ────────────────────────────────────────────────────────────────────────────────
   `spinbox'               x
  ────────────────────────────────────────────────────────────────────────────────
   `text'                  x
  ────────────────────────────────────────────────────────────────────────────────
   `tk'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_bisque'             -          only for tk backwards compatibility
  ────────────────────────────────────────────────────────────────────────────────
   `tk_chooseColor'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_chooseDirectory'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_dialog'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_focusFollowsMouse'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_focusNext'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_focusPrev'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_getOpenFile'        x
  ────────────────────────────────────────────────────────────────────────────────
   `tk_getSaveFile'        x
  ────────────────────────────────────────────────────────────────────────────────
   `tk_menuSetFocus'       -
  ────────────────────────────────────────────────────────────────────────────────
   `tk_messageBox'         x
  ────────────────────────────────────────────────────────────────────────────────
   `tk_optionMenu'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_popup'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_setPalette'         -
  ────────────────────────────────────────────────────────────────────────────────
   `tk_textCopy'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_textCut'
  ────────────────────────────────────────────────────────────────────────────────
   `tk_textPaste'
  ────────────────────────────────────────────────────────────────────────────────
   `tkerror'               -
  ────────────────────────────────────────────────────────────────────────────────
   `tkvars'                -
  ────────────────────────────────────────────────────────────────────────────────
   `tkwait'
  ────────────────────────────────────────────────────────────────────────────────
   `toplevel'              x
  ────────────────────────────────────────────────────────────────────────────────
   `winfo'                 x
  ────────────────────────────────────────────────────────────────────────────────
   `wm'                    x
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


  support of all config args as keywords to make-instance:

  ━━━━━━━━━━━━━━━━━━
   `bitmap'
  ──────────────────
   `button'       x
  ──────────────────
   `canvas'       x
  ──────────────────
   `checkbutton'  x
  ──────────────────
   `entry'        x
  ──────────────────
   `frame'        x
  ──────────────────
   `image'
  ──────────────────
   `label'        x
  ──────────────────
   `labelframe'   x
  ──────────────────
   `listbox'      x
  ──────────────────
   `menu'
  ──────────────────
   `menubutton'
  ──────────────────
   `message'
  ──────────────────
   `panedwindow'  x
  ──────────────────
   `photo'
  ──────────────────
   `radiobutton'  x
  ──────────────────
   `scale'        x
  ──────────────────
   `scrollbar'    x
  ──────────────────
   `spinbox'      x
  ──────────────────
   `text'         x
  ──────────────────
   `toplevel'     x
  ━━━━━━━━━━━━━━━━━━


4.1 Compatibility
─────────────────

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   OS / compiler   SBCL 1.4.14  ECL  CCL
  ───────────────────────────────────────
   Debian testing  x            ?    ?
  ───────────────────────────────────────
   MacOS           ?            ?    ?
  ───────────────────────────────────────
   Win             ?            ?    ?
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


5 License
═════════

  This software is Copyright (c) 2003-2010 Peter Herth
  <herth@peter-herth.de> Portions Copyright (c) 2005-2010 Thomas
  F. Burdick Portions Copyright (c) 2006-2010 Cadence Design Systems
  Portions Copyright (c) 2010 Daniel Herring Portions Copyright (c) 2018
  cage

  The authors grant you the rights to distribute and use this software
  as governed by the terms of the Lisp Lesser GNU Public License
  ([http://opensource.franz.com/preamble.html]), known as the LLGPL.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
