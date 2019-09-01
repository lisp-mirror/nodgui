                                ━━━━━━━━
                                 NODGUI
                                ━━━━━━━━


Table of Contents
─────────────────

1. NODGUI
2. Dependency
.. 1. Programs
.. 2. Lisp libraries
.. 3. TCL/TK libraries (optional)
3. Widget pack
.. 1. MegaWidget
.. 2. MegaWidget list
.. 3. TKlib
..... 1. TKlib derived widgets
4. FAQ
5. Status
.. 1. Compatibility
6. Notes
.. 1. Colors Name
7. License





1 NODGUI
════════

  /"No Drama GUI"/

  Common Lisp bindings for the Tk GUI toolkit.


2 Dependency
════════════

2.1 Programs
────────────

  • TCL/TK interpreter (version >= 8.6) <https://www.tcl.tk>


2.2 Lisp libraries
──────────────────

  • alexandria;
  • cl-ppcre-unicode;
  • cl-lex;
  • cl-yacc;
  • clunit2;
  • cl-colors2;
  • named-readtables.

  All of the above libraries are available on quicklisp.


2.3 TCL/TK libraries (optional)
───────────────────────────────

  • TKlib <https://core.tcl-lang.org/tklib/home>


3 Widget pack
═════════════

  This library define a few additional widgets than the standard TK
  ones.

  There should be a decent quantity of documentation as docstrings on
  the sources.

  Probably checking [the excellent quickdocs] or [the excellent as well
  quickref] could be useful too (note that the documentation there does
  not always reflects the latest version of this library).


[the excellent quickdocs] <http://quickdocs.org/nodgui/>

[the excellent as well quickref]
<https://quickref.common-lisp.net/nodgui.html>

3.1 MegaWidget
──────────────

  All these widget, written in common lisp, are defined in file
  `src/nogui-mw.lisp'.


3.2 MegaWidget list
───────────────────

  history-entry
        An entry widget keeping the history of previous input (which can
        be browsed through with cursor up and down), the input can be
        also autocompleted pressing the TAB key.


  treelist
        Display a tree-like structure by a set of listboxes.

        [screenshot]

  tooltip
        Display a little informative box for the widget under the mouse
        pointer position.

        [screenshot]

        [License for the image].

  gtree
        Render a tree.

        [screenshot]

  searchable-listbox
        A [listbox] with an entry to filter its contents

  autocomplete-listbox
        Acts like 'searchable-listbox' but the items added comes from
        the results from apply 'autocomplete-function-hook' to the
        content of the text entry.  This function is triggered after a
        character is inserted into the entry.

  date-picker
        A widget to choose a date

        [screenshot]

  password-entry
        A widget to input a password (the actual characters are not
        shown when typed, a placeholder is rendered instead).

  progress-bar-star
        a progress bar that display stars to show progress

        [screenshot]


[screenshot]
<https://www.autistici.org/interzona/img/nodgui/tree-list.png>

[screenshot]
<https://www.autistici.org/interzona/img/nodgui/tooltip.png>

[License for the image]
<https://notabug.org/cage/fulci/src/master/LICENSES.org>

[screenshot]
<https://www.autistici.org/interzona/img/nodgui/graphical-tree.png>

[listbox] <https://www.tcl.tk/man/tcl8.6/TkCmd/listbox.htm#M12>

[screenshot]
<https://www.autistici.org/interzona/img/nodgui/date-picker.png>

[screenshot]
<https://www.autistici.org/interzona/img/nodgui/progress-bar-star.png>


3.3 TKlib
─────────

  Some more widget in `nodgui' derive from wrapping and, in some case,
  extending the widget set contained in [TKlib]

  To use these widget in `nodgui' `tklib' must be installed on your
  system, fortunately this library is widely available at least on
  `GNU/Linux'.

  On Debian just a couple of commands are needed:

  ┌────
  │
  │ $ su -
  │ (root)# apt-get install tklib
  │
  └────


[TKlib] <https://core.tcl-lang.org/tklib/home>

3.3.1 TKlib derived widgets
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  calendar
        A widget to choose a date;
  notify-window
        show a fading out notification text to the user; on the top
        right of the screen;
  dot-plot
        draw a scatter plot (with optional error bars) on a canvas.


4 FAQ
═════


  Where is the documentation?
        nodgui is just the results of a code refactoring of LTK with
        some syntax sugar on top.  This means that the [original
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

        The rest of the documentation are the rare docstrings at the
        moment.

        If you wants to help please [open a pull request].

  Where can i found documentation about a widget?
        It is actually very simple: let's suppose you want to use a
        spinbox widget in your program and you have no idea how to do,
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
           configuration variable that you can pass as initarg for
           `(make instance 'spinbox ...)' and, sometimes, changed after
           object instancing via `configure' ([see the general
           documentation])

        2. for the meaning of this symbols refers to the [original TK
           documentation]

           As a general rule choose the link to the command with its
           name prefixed with "ttk::" (if exists).

           In our example point to:

           <https://www.tcl.tk/man/tcl8.6/TkCmd/ttk_spinbox.htm>

           and *not*

           <https://www.tcl.tk/man/tcl8.6/TkCmd/spinbox.htm>

           You can easily understand what all the options above are for.

  How can I access the root window?
        The root window is bound to the special variable `*tk*'.

  I have  a bunch of widgets  placed in a  grid and I want  those to scale themselves when the window is resized, there is a way to accomplish this?
        Sure! Use `grid-(column|row)-configure'.

        ┌────
        │ (grid-columnconfigure *tk* :all :weight 1)
        │ (grid-rowconfigure    *tk* :all :weight 1)
        └────

        You can replace `:all' with a row or column indices to apply
        this behaviour only to a subset of the columns or rows.

  What are the differences between nodgui and LTK?
        The two libraries begin to diverge a bit now, the API is
        compatible with LTK no more.

        1. A reader macro for events i.e.  `#$<Alt-q>$' instead of
           `"<Alt-q>"' (a string), the macro will parse and check for
           trivial errors in the event definition syntax at compile
           time.
           ┌────
           │ (named-readtables:in-readtable nodgui.syntax:nodgui-syntax) ; do not forget that!
           │
           │ [...]
           │
           │ (bind *tk* #$<Alt-q>$ (lambda (event) ...
           └────

           /Please note that a string is still accepted as event
           specifier./

        2. A DSL for TCL code that allow to mix TCL code and lisp
           expression, check for example the following code to create a
           bitmap:

           ┌────
           │ (named-readtables:in-readtable nodgui.syntax:nodgui-syntax)
           │
           │ [...]
           │
           │ (tclize `([list
           │             ,#[loop for r from 0 below h collect
           │                 (tclize `([list ,#[loop
           │ ...
           └────

           The `tclize' macro will transforms the backquoted forms in
           TCL code, the unquoted forms will be evaluated before the
           translation occurs.

           Please always wrap the unquoted form in `#[ ... 'a space' ]'
           because this will force escaping of the data (e.g.  from:
           `'{'' to `\{').

           You should put a space before the closing square brackets,
           this is an annoying bug that will be solved soon or later.

        3. The function [`postscript'] returns a postscript file as
           string instead of write a file on disk;

        4. Support for images in GIF, PNG, TGA, JPEG and raw RGB format,
           limited manipulation for the latest three is provided
           (scaling and rotation).

        5. Compatible only with TK 8.6;

        6. A general refactoring.

        7. Integrated with cl-colors library (can use X11 color names or
           rgb struct, see 6.1).

        8. Some bugs fixed (and more added of course :-) )

  Where can i find more code examples?
        check the file `src/demo-tests.lisp'

  Can I contribute to this project?
        Yes, of course! Please open an issue or a pull request on the
        [web repository], if you do not feel comfortable with coding
        documentation improvements are very welcome too! :)

        Also i would appreciate [testing] if the library works on
        different environment than mine (Debian GNU/Linux with SBCL).


[original documentation] <http://www.peter-herth.de/ltk/ltkdoc/>

[Introduction] <http://www.peter-herth.de/ltk/ltkdoc/node2.html>

[Tutorial] <http://www.peter-herth.de/ltk/ltkdoc/node4.html>

[Event handling] <http://www.peter-herth.de/ltk/ltkdoc/node14.html>

[Screen functions] <http://www.peter-herth.de/ltk/ltkdoc/node41.html>

[Window manager functions]
<http://www.peter-herth.de/ltk/ltkdoc/node42.html>

[Under the hood] <http://www.peter-herth.de/ltk/ltkdoc/node43.html>

[the widgets] <http://www.peter-herth.de/ltk/ltkdoc/node17.html>

[open a pull request] <https://notabug.org/cage/nodgui/issues>

[see the general documentation] See figure (4)

[original TK documentation]
<https://www.tcl.tk/man/tcl8.6/TkCmd/contents.htm>

[`postscript'] <https://www.tcl.tk/man/tcl8.6/TkCmd/canvas.htm#M61>

[web repository] <https://notabug.org/cage/nodgui>

[testing] See section 5.1


5 Status
════════

  All tk commands as of version 8.4 with support information. "-" means
  not supported by purpose (look comment), "x" means supported, though
  some options may not be supported.

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   command                 supported  comment
  ──────────────────────────────────────────────────────────────────────────────────────
   `bell'                  x
  ──────────────────────────────────────────────────────────────────────────────────────
   `bind'                  x
  ──────────────────────────────────────────────────────────────────────────────────────
   `bindtags'              modify     modify the tag list of a widget that
                                      describes which events it gets
  ──────────────────────────────────────────────────────────────────────────────────────
   `bitmap'                -          see image
  ──────────────────────────────────────────────────────────────────────────────────────
   `button'                x
  ──────────────────────────────────────────────────────────────────────────────────────
   `canvas'                x
  ──────────────────────────────────────────────────────────────────────────────────────
   `checkbutton'           x
  ──────────────────────────────────────────────────────────────────────────────────────
   `clipboard'             x          (canvas get missing… tricky…)
  ──────────────────────────────────────────────────────────────────────────────────────
   `colors'                -          see 6.1
  ──────────────────────────────────────────────────────────────────────────────────────
   `console'               -          only on some platforms
  ──────────────────────────────────────────────────────────────────────────────────────
   `cursors'               x
  ──────────────────────────────────────────────────────────────────────────────────────
   `destroy'               x
  ──────────────────────────────────────────────────────────────────────────────────────
   `entry'                 x
  ──────────────────────────────────────────────────────────────────────────────────────
   `event'                 create     create and manage virtual events
  ──────────────────────────────────────────────────────────────────────────────────────
   `focus'                 x          focus management functions
  ──────────────────────────────────────────────────────────────────────────────────────
   `font'
  ──────────────────────────────────────────────────────────────────────────────────────
   `frame'                 x
  ──────────────────────────────────────────────────────────────────────────────────────
   `grab'
  ──────────────────────────────────────────────────────────────────────────────────────
   `busy'                  x
  ──────────────────────────────────────────────────────────────────────────────────────
   `grid'                  x
  ──────────────────────────────────────────────────────────────────────────────────────
   `image'                 x
  ──────────────────────────────────────────────────────────────────────────────────────
   `keysyms'               x
  ──────────────────────────────────────────────────────────────────────────────────────
   `label'                 x
  ──────────────────────────────────────────────────────────────────────────────────────
   `labelframe'            x
  ──────────────────────────────────────────────────────────────────────────────────────
   `listbox'               x
  ──────────────────────────────────────────────────────────────────────────────────────
   `loadTk'                -
  ──────────────────────────────────────────────────────────────────────────────────────
   `lower'                 x
  ──────────────────────────────────────────────────────────────────────────────────────
   `menu'                  x
  ──────────────────────────────────────────────────────────────────────────────────────
   `menubutton'            x
  ──────────────────────────────────────────────────────────────────────────────────────
   `message'               x
  ──────────────────────────────────────────────────────────────────────────────────────
   `option'                -
  ──────────────────────────────────────────────────────────────────────────────────────
   `options'               -          only helpfile
  ──────────────────────────────────────────────────────────────────────────────────────
   `pack'                  x
  ──────────────────────────────────────────────────────────────────────────────────────
   `panedwindow'           x
  ──────────────────────────────────────────────────────────────────────────────────────
   `photo'                 x          support for PNG, GIF, JPEG and raw RGB(A) format.
  ──────────────────────────────────────────────────────────────────────────────────────
   `place'                 x          geometry manager using coordinates
  ──────────────────────────────────────────────────────────────────────────────────────
   `radiobutton'           x
  ──────────────────────────────────────────────────────────────────────────────────────
   `raise'                 x
  ──────────────────────────────────────────────────────────────────────────────────────
   `scale'                 x
  ──────────────────────────────────────────────────────────────────────────────────────
   `scrollbar'             x
  ──────────────────────────────────────────────────────────────────────────────────────
   `selection'
  ──────────────────────────────────────────────────────────────────────────────────────
   `send'
  ──────────────────────────────────────────────────────────────────────────────────────
   `spinbox'               x
  ──────────────────────────────────────────────────────────────────────────────────────
   `text'                  x
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_bisque'             -          only for tk backwards compatibility
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_chooseColor'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_chooseDirectory'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_dialog'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_focusFollowsMouse'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_focusNext'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_focusPrev'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_getOpenFile'        x
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_getSaveFile'        x
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_menuSetFocus'       -
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_messageBox'         x
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_optionMenu'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_popup'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_setPalette'         -
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_textCopy'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_textCut'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tk_textPaste'
  ──────────────────────────────────────────────────────────────────────────────────────
   `tkerror'               -
  ──────────────────────────────────────────────────────────────────────────────────────
   `tkvars'                -
  ──────────────────────────────────────────────────────────────────────────────────────
   `tkwait'
  ──────────────────────────────────────────────────────────────────────────────────────
   `toplevel'              x
  ──────────────────────────────────────────────────────────────────────────────────────
   `treeview'              x
  ──────────────────────────────────────────────────────────────────────────────────────
   `winfo'                 x
  ──────────────────────────────────────────────────────────────────────────────────────
   `wm'                    x
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


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


5.1 Compatibility
─────────────────

  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
   OS / compiler  SBCL 1.4.14  ECL   CCL 1.11.6
  ──────────────────────────────────────────────
   GNU/Linux      x            x[1]  x
  ──────────────────────────────────────────────
   MacOS          ?            ?     x[2]
  ──────────────────────────────────────────────
   Win            ?            ?     ?
  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━


6 Notes
═══════

6.1 Colors Name
───────────────

  Color name from library cl-color can be used as follows:

  • with a reader macro (`#%...%') at read time:
    ┌────
    │ (named-readtables:in-readtable nodgui.syntax:nodgui-syntax) ; do not forget that!
    │
    │ [...]
    │
    │ #%red%
    └────

    at runtime using:

    ┌────
    │ (rgb->tk cl-colors:+red+)
    └────

    the list of supported colors name can be found in: [this file].


[this file]
<https://notabug.org/cage/cl-colors2/src/master/package.lisp>


7 License
═════════

  This software is Copyright (c) 2003-2010 Peter Herth
  <herth@peter-herth.de> Portions Copyright (c) 2005-2010 Thomas
  F. Burdick Portions Copyright (c) 2006-2010 Cadence Design Systems
  Portions Copyright (c) 2010 Daniel Herring Portions Copyright (c)
  2018,2019 cage

  The authors grant you the rights to distribute and use this software
  as governed by the terms of the Lisp Lesser GNU Public License
  (<http://opensource.franz.com/preamble.html>), known as the LLGPL.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.



Footnotes
─────────

[1] Using version from git repository

[2] It requires TCL/TK update see: [issue #7]
(<https://notabug.org/cage/nodgui/issues/7>)
