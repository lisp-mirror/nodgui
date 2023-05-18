                                ━━━━━━━━
                                 NODGUI
                                ━━━━━━━━


Table of Contents
─────────────────

1. NODGUI
.. 1. important note:
2. Dependency
.. 1. Programs
.. 2. Lisp libraries
.. 3. TCL/TK libraries (optional)
3. Installing
4. Documentation
5. License
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
  Portions Copyright (c) 2010 Daniel Herring Portions Copyright (c)
  2018,2019,2020,2022 cage

  The authors grant you the rights to distribute and use this software
  as governed by the terms of the Lisp Lesser GNU Public License
  (<http://opensource.franz.com/preamble.html>), known as the LLGPL.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.


6 Acknowledgment
════════════════

  My (cage) deep thanks to Tim Holliefield <tholliefield [at]
  online.de>, for all the suggestions and help about implementing the
  widgets styles management.
