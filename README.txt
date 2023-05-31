                                ━━━━━━━━
                                 NODGUI
                                ━━━━━━━━


Table of Contents
─────────────────

1. NODGUI
.. 1. important note:
.. 2. Features
.. 3. Themes
2. Dependency
.. 1. Programs
.. 2. Lisp libraries
.. 3. TCL/TK libraries (optional)
3. Installing
4. Documentation
5. License
.. 1. Themes
6. Acknowledgment





1 NODGUI
════════

  /"No Drama GUI"/

  Common Lisp bindings for the Tk GUI toolkit.


1.1 important note:
───────────────────

  Version `0.4.0' broke the user API, see [here] for more information
  about how your code is affected and how to fix it.


[here] <https://www.autistici.org/interzona/nodgui.html#orgb470f4b>


1.2 Features
────────────

  • custom themes
  • more widgets:
    • history-entry
    • autocomplete
    • expanded "text" widget
    • date picker
    • treelist
    • tooltip
    • searchable listbox
    • password entry
    • …
  • `tklib'-derived widgets:
    • calendar
    • notify-window
    • plots and charts: dot-plot, bar-chart…
  • asynchronous main event loop
  • and more


1.3 Themes
──────────

  nodgui supports loading [custom ttk themes] and ships the "yaru" theme
  from the [ttkthemes collection].

  To use a built-in theme other than the "default" one, do:

  ┌────
  │ (with-nodgui (:theme "yaru")
  │    (code here))
  └────

  or use `(setf *default-theme* "yaru")'.

  You can see how they look with the demos:

  ┌────
  │ CL-USER> (nodgui.demo:demo :theme "yaru")
  └────

  You can load any .tcl file describing a theme:

  ┌────
  │ (nodgui:eval-tcl-file "path/to/another/theme.tcl")
  │ (nodgui:use-theme "theme")
  └────

  See also `*themes-directory*'.

  This currently doesn't work with a few themes using SVG images, but it
  might be fixed with the release of an upcoming Tcl/Tk version.


[custom ttk themes] <https://wiki.tcl-lang.org/page/List+of+ttk+Themes>

[ttkthemes collection]
<https://ttkthemes.readthedocs.io/en/latest/themes.html#yaru>


2 Dependency
════════════

2.1 Programs
────────────

  • TCL/TK interpreter (version >= 8.6) <https://www.tcl.tk>


2.2 Lisp libraries
──────────────────

  • alexandria;
  • cl-ppcre-unicode;
  • esrap
  • clunit2;
  • cl-colors2;
  • named-readtables.

  All of the above libraries are available on quicklisp.


2.3 TCL/TK libraries (optional)
───────────────────────────────

  • TKlib <https://core.tcl-lang.org/tklib/home>

  • tkimg <https://sourceforge.net/projects/tkimg/>


3 Installing
════════════

  1. install the tk interpreter (example on a Debian system follows)
  ┌────
  │ # apt-get install tk
  └────

  1. optionally install tklib
  ┌────
  │ # apt-get install tklib
  └────

  1. optionally install tkimg

  ┌────
  │ # apt-get install libtk-img
  └────

  1. open a REPL and type
  ┌────
  │ (ql:quickload "nodgui")
  └────

  1. try it!
  ┌────
  │ (nodgui.demo::demo)
  └────


4 Documentation
═══════════════

  please follow [this link for documentation and news].


[this link for documentation and news]
<https://www.autistici.org/interzona/nodgui.html>


5 License
═════════

  This software is Copyright (c) 2003-2010 Peter Herth
  <herth@peter-herth.de> Portions Copyright (c) 2005-2010 Thomas
  F. Burdick Portions Copyright (c) 2006-2010 Cadence Design Systems
  Portions Copyright (c) 2010 Daniel Herring Portions Copyright © cage

  The authors grant you the rights to distribute and use this software
  as governed by the terms of the Lisp Lesser GNU Public License
  (<http://opensource.franz.com/preamble.html>), known as the LLGPL.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.


5.1 Themes
──────────

  The themes definitions found in the `themes/' subdirectory are
  distributed with many different license, please carefully check the
  file `LICENSE.org' in each theme's subdirectory (e.g
  <./themes/yaru/LICENSE.org>) for more information.


6 Acknowledgment
════════════════

  My (cage) deep thanks to Tim Holliefield <tholliefield [at]
  online.de>, for all the suggestions and help about implementing the
  widgets styles management.
