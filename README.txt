                                ━━━━━━━━
                                 NODGUI
                                ━━━━━━━━


Table of Contents
─────────────────

1. Important note: This branch of NODGUI is highly experimental, API could change and even more bugs can appears.
2. NODGUI
3. Dependency
.. 1. Programs
.. 2. Lisp libraries
.. 3. TCL/TK libraries (optional)
4. Installing
5. Documentation
6. License





1 Important note: This branch of NODGUI is highly experimental, API could change and even more bugs can appears.
════════════════════════════════════════════════════════════════════════════════════════════════════════════════


2 NODGUI
════════

  /"No Drama GUI"/

  Common Lisp bindings for the Tk GUI toolkit.


3 Dependency
════════════

3.1 Programs
────────────

  • TCL/TK interpreter (version >= 8.6) <https://www.tcl.tk>


3.2 Lisp libraries
──────────────────

  • alexandria;
  • cl-ppcre-unicode;
  • cl-lex;
  • cl-yacc;
  • clunit2;
  • cl-colors2;
  • named-readtables.

  All of the above libraries are available on quicklisp.


3.3 TCL/TK libraries (optional)
───────────────────────────────

  • TKlib <https://core.tcl-lang.org/tklib/home>


4 Installing
════════════

  1. install the tk interpreter (example on a Debian system follows)
  ┌────
  │ # apt-get install tk
  └────

  1. optionally install tklib
  ┌────
  │ # apt-get install tklib
  └────

  1. open a REPL and type
  ┌────
  │ (ql:quickload "nodgui")
  └────

  1. try it!
  ┌────
  │ (nodgui.demo::demo)
  └────


5 Documentation
═══════════════

  please follow [this link for documentation and news].


[this link for documentation and news]
<https://www.autistici.org/interzona/nodgui.html>


6 License
═════════

  This software is Copyright (c) 2003-2010 Peter Herth
  <herth@peter-herth.de> Portions Copyright (c) 2005-2010 Thomas
  F. Burdick Portions Copyright (c) 2006-2010 Cadence Design Systems
  Portions Copyright (c) 2010 Daniel Herring Portions Copyright (c)
  2018,2019,2020 cage

  The authors grant you the rights to distribute and use this software
  as governed by the terms of the Lisp Lesser GNU Public License
  (<http://opensource.franz.com/preamble.html>), known as the LLGPL.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.
