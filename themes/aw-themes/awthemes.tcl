#!/usr/bin/tclsh
#
# awdark and awlight themes
# scalable black, winxpblue, breeze, arc, clearlooks themes.
#
# See 'awtemplate.tcl' to create a new theme.
#
# Copyright 2018 Brad Lanam Walnut Creek, CA
# Copyright 2019-2020 Brad Lanam Pleasant Hill, CA
#
# zlib/libpng license
#
# Helper routines:
#
#   ::ttk::theme::${theme}::setMenuColors {.menuwidget|-optiondb}
#     Sets the menu colors and also changes any checkbutton and
#     radiobutton types to use thematic images.
#     Side effect: The menu will have -hidemargin set to true.
#
#     If -optiondb is specified instead of a widget, the *Menu.*
#     color configurations are changed in the options database.
#
#   ::ttk::theme::${theme}::setTextColors \
#         {.textwidget|-optiondb} [{-entry|-background}]
#     Sets the text widget colors.
#
#     If the text widget is in a "normal" state, the background is
#     set to the entry background.  If the text widget is in a
#     "disabled" state, the background is set to the background color.
#
#     If -entry is specified, the background is set the same as
#     an entry widget's background.  This is needed when -optiondb
#     is specified, or to override the normal/disabled logic.
#
#     If -background is specified, the background is set the same as
#     the normal background.  This can be used to override the
#     normal/disabled logic.
#
#     If -optiondb is specified instead of a widget, the *Text.*
#     color configurations are changed in the options database.
#
#   ::ttk::theme::${theme}::setListboxColors {.listboxwidget|-optiondb}
#     Sets the listbox widget colors.
#
#     If -optiondb is specified instead of a widget, the *Listbox.*
#     color configurations are changed in the options database.
#
#   ::ttk::theme::${theme}::scaledStyle \
#           prefix defaultfont listingfont
#     Creates a set of scaled widgets with a new style name.
#     Arguments:
#         prefix: prefix for the name of the new style
#         defaultfont: the standard font name (default font size)
#         listingfont: the font name to scale to.
#
#     See the example in 'demoscaled.tcl'.
#
#   ::themeutils::setBackgroundColor <theme> <color>
#     Changes the background color.
#
#     Note that this routine must be called before the theme is initialized.
#     It does not work after the theme is initialized.
#
#       package require awthemes
#       ::themeutils::setBackgroundColor awdark #001122
#       package require awdark
#
#     To change the background color after the theme is instantiated, use:
#
#       set theme [::ttk::style theme use]
#       if { [info command ::ttk::theme::${theme}::setBackground] ne {} } {
#         ::ttk::theme::${theme}::setBackground $newcolor
#       }
#
#   ::themeutils::setHighlightColor <theme> <color>
#     Changes the graphics, focus and selection background colors.
#
#     Note that this routine MUST be called before the theme is initialized.
#     The graphics are initialized when the theme is instantiated, and the
#     graphics colors cannot be changed afterwards.
#
#       package require awthemes
#       ::themeutils::setHighlightColor awdark #007000
#       package require awdark
#
#     Changes the graphics, focus and select background colors.
#
#   ::ttk::theme::${theme}::getScaleFactor [scaled-style-prefix]
#     Gets the scaling factor.  For a scaled style, specify the prefix name.
#     For use by the end user so that other images can be scaled appropriately.
#
#   ::themeutils::setThemeColors <theme> {<colorname> <color> ...}
#     A lower level routine that allows modification of any of the colors.
#
#     Allows modification of any of the colors used by awdark and awlight.
#     The graphical colors will be changed to match.
#     e.g.
#
#       package require awthemes
#       ::themeutils::setThemeColors awdark \
#             graphics.color #007000 \
#             focus.color #007000
#       package require awdark
#     will change the graphical and focus color to a green.
#
#     To change the user scaling:
#       awthemes uses the [tk scaling] factor multiplied by
#         the user scaling factor to determine the graphics scaling factor.
#
#       package require awthemes
#       ::themeutils::setThemeColors awdark \
#             scale.factor 1.5
#       package require awdark
#
#   ::ttk::theme::${theme}::setBackground <color>
#       Used after the theme has been instantiated.
#
#       set theme [::ttk::style theme use]
#       if { [info command ::ttk::theme::${theme}::setBackground] ne {} } {
#         ::ttk::theme::${theme}::setBackground $newcolor
#       }
#
#   ::ttk::theme::${theme}::getColor <theme> <colorname>
#     A lower level procedure to get the color assigned to a theme colorname.
#
# Change History
#
# 10.4.0  2021-06-18
#   - awdark/awlight : change to use the solid widget theme for combobox
#       arrows.  This fixes scaling issues when the combobox font is changed.
#   - Added combobox.color.arrow option.
#   - Fix incorrect colors in arrow/solid widget theme.
#   - Fix incorrect combobox/solid-bg settings.tcl.
# 10.3.2  2021-06-11
#   - Handle ::notksvg properly for 8.7
#   - Use tk version, not tcl version for 8.7 checks.
#   - Fix package vcompare.
# 10.3.1  2021-06-10
#   - Check for Tcl version 8.7
#   - Update check for svg image support.
# 10.3.0  2021-03-22
#   - Add awbreezedark by Bartek Jasicki
#   - Add active.color color for use by some widget themes.
#   - Internal changes to support active color.
#   - Fixed checkbutton width issues.
#   - Cleaned up treeview chevron widget theme.
# 10.2.1 (2021-02-11)
#   - Set text area -insertbackground so that the cursor has the proper color.
# 10.2.0 (2021-01-02)
#   - Add 'getScaleFactor' procedure so that the user can scale
#     their images appropriately.
# 10.1.2 (2020-12-20)
#   - Production Release of 10.1.1 and 10.1.0
# 10.1.1 (2020-12-15) ** Pre-release **
#   - Menus: add support for menu foreground (menu.fg).
#   - Option database initialization: Do not initialize the menu colors
#     on Windows.  Using 'setMenuColors' on Windows leaves the top menubar
#     a light color, and the menu colors dark with a large border.
#     Use: ::ttk::theme::${theme}::setMenuColors -optiondb
#     to apply anyways.
# 10.1.0 (2020-12-14) ** Pre-release **
#   - setTextColors: Set text foreground colors appropriately.
#   - Toolbutton: set selected color.
#   - Menus: add support for menu relief (menu.relief).  Default to 'raised'.
#     Always keep the borderwidth set to 1, unscaled.
#   - Menus: change background color for menus to a darker color.
#   - Listbox: change -activestyle to none.
# 10.0.0 (2020-12-2)
#   - option database is always updated.  The text widget colors will
#     default to -entry.
#   - add ttk::theme::<theme> package names so that the option db can
#     be used to set the theme and the old setTheme and ttk::themes
#     procedures may be used.
#   - Breaking change:
#     Theme name changes to prevent conflicts with the originals.
#     arc -> awarc, black -> awblack, breeze -> awbreeze,
#     clearlooks -> awclearlooks, winxpblue -> awwinxpblue.
#     Required due to the addition of the ttk::theme::<theme> package names.
#   - Added manual page.
# 9.5.1.1 (2020-11-16)
#   - update licensing information
# 9.5.1 (2020-11-10)
#   - progressbar/rect-bord: fix: set trough image border.
#   - setMenuColors: add ability to set the option database.
#   - setTextColors: add ability to set the option database.
#   - setListboxColors: add ability to set the option database.
#   - setMenuColors: change selectColor to use fg.fg (for option database).
#   - setTextColors: add -background option.
#   - setTextColors: deprecate -dark option.
# 9.5.0 (2020-10-29)
#   - Fix so that multiple scaled styles will work.
#   - Change so that scaled styles can have (a few of) their own colors.
#   - Code cleanup
# 9.4.2 (2020-10-23)
#   - Renamed internal color names.
#     This may break backwards compatibility for anyone using
#     'setThemeColors' or 'getColor'.
#   - removed 'setThemeGroupColor' function.
#   - Fix so that a missing or incorrect widget style will fallback
#     to 'none' and use the parent theme's style.
#   - breeze, arc: fix active vertical scale handle.
#   - Added $::themeutils::awversion to allow version checks.
#   - Fix scalable themes so that they will fail to load if tksvg is
#     not present.
#   - Improve scaling/layout of combobox/solid-bg.
#   - demottk.tcl: added 'package require' as a method to load the themes.
#   - clean demo code before production releases.
# 9.4.1 (2020-10-16)
#   - fix mkpkgidx.sh script for clearlooks theme.
# 9.4.0 (2020-10-16)
#   - added scalable clearlooks theme.
#   - arc: improved some colors. fixed tab height.
#   - scrollbar style: Fix so that a separate scrollbar slider style
#     can be set, but still uses the progressbar/ directory.
#   - arrow/solid, arrow/solid-bg, combobox/solid-bg: reduce arrow height.
#   - treeview heading: improve colors.
#   - setTextColors: set background color appropriately if the widget
#     is in a normal state.
#   - renamed scale/rect-bord-circle to scale/rect-bord-grip.  clean up.
#   - progressbar/rect-bord: clean up.
#   - combobox/rounded: new widget style.
#   - progressbar/rect-diag: new widget style.
#   - button/roundedrect-gradient: new widget style.
#   - scale/rect-narrow: new scale/scale-trough widget style.
#   - awdark/awlight: no tksvg: improved/fixed arrow colors.
#   - demottk.tcl: beta: added a tablelist tab if tablelist 6.11+ is available.
#   - demottk.tcl: added an '-autopath' option.
# 9.3.2 (2020-10-5)
#   - setListboxColors: Fixed to properly set colors on
#     removal/reinstantiation of a listbox.
#   - Minor code cleanup.
#   - setTextColors: Removed configuration of border width.
# 9.3.1 (2020-9-17)
#   - Remove debug.
# 9.3 (2020-9-17)
#   - Fixed inappropriate toolbutton width setting.
# 9.2.4 (2020-8-14)
#   - remove unneeded options for scrollbar
# 9.2.3 (2020-7-17)
#   - remove focus ring from treeview selection.
# 9.2.2 (2020-6-6)
#   - fix: settextcolors: background color.
# 9.2.1 (2020-5-20)
#   - fix: progressbar: rect, rect-bord border size.
# 9.2 (2020-4-30)
#   - arc: notebook: use roundedtop-dark style.
#   - fix: set of background/highlight colors: remove extra adjustment.
#   - fix: setBackground() color adjustments.
# 9.1.1 (2020-4-27)
#   - fix package provide statements.
# 9.1 (2020-4-27)
#   - progressbar: rect-bord: fixed sizing.
#   - progressbar: rect-bord: removed border on trough.
#   - various fixes for all themes.
#   - Added 'arc' theme by Sergei Golovan
# 9.0 (2020-4-23)
#   - added 'awtemplate.tcl' as an example to start a new theme.
#   - simplified and regularized all images.
#   - reworked color settings, made much easier to use.
#   - reworked all radiobuttons and checkbuttons.
#   - treeview: added selected arrow right and selected arrow down images.
#   - arrows: added solid, open triangle styles.
#   - progressbar: rounded-line: reduced width (breeze).
#   - various fixes and improvements to all themes.
#   - fix combobox listbox handler.
#   - fix combobox color mappings.
# 8.1 (2020-4-20)
#   - rename all colors names so that they can be grouped properly
#   - fix: slider/trough display (padding).
#   - fix: incorrect combobox state coloring.
#   - fix background changes so that it only modifies the
#     properly associated background colors.
#   - added helper routine 'setThemeGroupColor'
#   - changed 'setHighlightColor' to also change the select background color.
# 8.0 (2020-4-18)
#   - menu radiobuttons and menu checkbuttons are now dynamically generated
#     and any corresponding .svg files have been removed.
#     This also fixes menu radio/check button sizing issues for themes
#     other than awdark and awlight.
#   - treeview arrows default to inheriting from standard arrows.
#   - The themes have been reworked such that each widget has different
#     styles that can be applied.  All widget styles are now found in
#     the i/awthemes/ directory, and individual theme directories are no
#     longer needed.  A theme's style may be overridden by the user.
#   - style: slider/rect-bord: cleaned up some sizing issues
#     (awdark/awlight)
#   - style: arrow/solid-bg: cleaned up some sizing issues (awdark/awlight)
#   - fix: disabled progressbar display.
#   - fix: disabled trough display.
# 7.9
#   - winxpblue: fixed minor focus color issues (entry, combobox).
#   - fixed incorrect scrollbar background color.
#   - button: added state {active focus}.
#   - entry: added ability to set graphics.
#   - notebook: added hover, disabled graphics.
#   - combobox: graphics will be set if entry graphics are present.
#   - combobox: readonly graphics will be set to button graphics if
#     both entry and button graphics are present (breeze theme).
#   - menubutton: option to use button graphics for menubuttons.
#   - toolbutton: option to use button graphics for toolbuttons.
#   - setListBoxColors: remove borderwidth and relief settings.
#   - spinbox: graphics will be set if entry graphics are present.
#   - internal code cleanup: various theme settings have been renamed.
#   - added breeze theme (based on Maximilian Lika's breeze theme 0.8).
#   - add new helper routines to ::themeutils to set the background color
#     and to set the focus/highlight color.
#   - awdark/awlight: no tksvg: Fixed some grip/slider colors.
#   - fix user color overrides
# 7.8 (2020-308)
#   - set listbox and text widget highlight color, highlight background color.
# 7.7 (2020-1-17)
#   - fix crash when tksvg not present.
#   - improve awdark border colors.
# 7.6 (2019-12-7)
#   - better grip design
# 7.5 (2019-12-4)
#   - reworked all .svg files.
#   - cleaned up notebook colors.
#   - fixed scaling issue with scaled style scaling.
#   - fixed combobox scaling.
#   - fixed scrollbar arrows.
#   - scaled combobox listbox scrollbar.
#   - added hasImage routine
# 7.4 (2019-12-3)
#   - added getColor routine for use by checkButtonToggle
#   - Fix menu highlight color
# 7.3 (2019-12-2)
#   - fix spinbox scaled styling
# 7.2 (2019-12-2)
#   - setBackground will not do anything if the background color is unchanged.
#   - fixed a bug with graphical buttons.
#   - make setbackground more robust.
# 7.1 (2019-12-1)
#   - fix border/padding scaling, needed for rounded buttons/tabs.
# 7.0 (2019-11-30)
#   - clean up .svg files to use alpha channel for disabled colors.
#   - calculate some disabled colors.
#   - fix doc.
#   - split out theme specific code into separate files.
#   - Fix scaledStyle set of treeview indicator.
#   - make the tab topbar a generalized option.
#   - merge themeutils package
#   - clean up notebook tabs.
#   - winxpblue: notebook tab graphics.
#   - winxpblue: disabled images.
#   - black: disabled cb/rb images.
#   - black: add labelframe color.
# 6.0 (2019-11-23)
#   - fix !focus colors
#   - slider border color
#   - various styling fixes and improvements
#   - separate scrollbar color
#   - optional scrollbar grip
#   - button images are now supported
#   - added winxpblue scalable theme
#   - fixed missing awdark and awlight labelframe
# 5.1 (2019-11-20)
#   - add more colors to support differing spinbox and scroll arrow colors.
#   - awlight, awdark, black theme cleanup
#   - rename menubutton arrow .svg files.
#   - menubutton styling fixes
# 5.0 (2019-11-19)
#   - rewrite so that the procedures are no longer duplicated.
#   - rewrite set of arrow height/width and combobox arrow height.
#   - Add scaledStyle procedure to add a scaled style to the theme.
#   - Added a user configurable scaling factor.
# 4.2.1
#   - rewrite pkgIndex.tcl
# 4.2
#   - fix scaling of images.
#   - adjust sizing for menu checkbutton and radiobutton.
#   - add support for flexmenu.
# 4.1
#   - breaking change: renamed tab.* color names to base.tab.*
#   - fix bugs in setBackground and setHighlight caused by the color
#       renaming.
#   - fix where the hover color for check and radio buttons is set.
# 4.0
#   - add support for other clam based themes.
#   - breaking change: the .svg files are now loaded from the filesystem
#       in order to support multiple themes.
#   - breaking change: All of the various colors and derived colors have
#       been renamed.
#   - awdark/awlight: Fixed empty treeview indicator.
#   - added scalable 'black' theme.
# 3.1
#   - allow user configuration of colors
# 3.0
#   - tksvg support
# 2.6
#   - Fix mac colors
# 2.5
#   - Added missing TFrame style setup.
# 2.4
#   - Some cleanup for text field background.
# 2.3
#   - Added padding for Menu.TCheckbutton and Menu.TRadiobutton styles
# 2.2
#   - Added support for flexmenu.
#   - Fixed listbox frame.
# 2.1
#   - Added Menu.TCheckbutton and Menu.TRadiobutton styles
#     to support menu creation.
# 2.0
#   - Add setBackground(), setHighlight() routines.
#     If wanted, these require the colorutils package.
#   - Removed the 'option add' statements and use a bind
#     to set the combobox's listbox colors.
#   - Merge awdark and awlight themes into a single package.
#   - More color cleanup.
#   - Make notebook top bar use dynamic colors.
# 1.4
#   - change button anchor to default to center to follow majority of themes
# 1.3
#   - clean up images a little.
# 1.2
#   - fix button press color
# 1.1
#   - per request. remove leading :: for the package name.
# 1.0
#   - more fixes for mac os x.
# 0.9
#   - changes to make it work on mac os x
# 0.8
#   - set disabled field background color for entry, spinbox, combobox.
#   - set disabled border color
# 0.7
#   - add option to text helper routine to set the darker background color.
#   - fix listbox helper routine
# 0.6
#   - fixed hover color over notebook tabs
# 0.5
#   - menubutton styling
#   - added hover images for radio/check buttons
# 0.4
#   - paned window styling
#   - clean up disabled widget color issues.
# 0.3
#   - set disabled arrowcolors
#   - fix namespace
#   - fix disabled check/radio button colors
#   - set color of scale grip and scrollbar grip to dark highlight.
# 0.2
#   - reduced height of widgets
#   - labelframe styling
#   - treeview styling
#   - remove extra outline color on notebook tabs
#   - fix non-selected color of notebook tabs
#   - added setMenuColors helper routine
#   - cleaned up the radiobutton image
#   - resized checkbutton and radiobutton images.
# 0.1
#   - initial coding
#
#
# NOTES:
#   It is difficult to create gradient buttons.  If the ns value of the
#   image border goes over 8, Tk will fail and lock up.
#
#

namespace eval ::themeutils {}
set ::themeutils::awversion 10.4.0
package provide awthemes $::themeutils::awversion

package require Tk
# set ::notksvg to true for testing purposes
# package vcompare is technically correct, not useful,
# so use 8.6.99
if { (! [info exists ::notksvg] || ! $::notksvg) &&
    [package vcompare 8.6.99 $::tk_version] > 0 } {
  catch { package require tksvg }
}

try {
  set iscript [info script]
  if { [file type $iscript] eq "link" } { set iscript [file link $iscript] }
  set ap [file normalize [file dirname $iscript]]
  if { $ap ni $::auto_path } {
    lappend ::auto_path $ap
  }
  set ap [file normalize [file join [file dirname $iscript] .. code]]
  if { $ap ni $::auto_path } {
    lappend ::auto_path $ap
  }
  unset ap
  unset iscript
  package require colorutils
} on error {err res} {
  puts stderr "ERROR: colorutils package is required"
}

namespace eval ::ttk::awthemes {
  set debug 0
  set starttime 0
  set endtime 0

  proc init { theme } {
    set gstarttime [clock milliseconds]

    namespace eval ::ttk::theme::${theme} {
      variable vars
      variable colors
      variable images
      variable rawimgdata
      variable imgdata
      variable imgtype
    }

    # set up some aliases for the theme
    interp alias {} ::ttk::theme::${theme}::scaledStyle {} ::ttk::awthemes::scaledStyle
    interp alias {} ::ttk::theme::${theme}::setBackground {} ::ttk::awthemes::setBackground
    interp alias {} ::ttk::theme::${theme}::setHighlight {} ::ttk::awthemes::setHighlight
    interp alias {} ::ttk::theme::${theme}::setListboxColors {} ::ttk::awthemes::setListboxColors
    interp alias {} ::ttk::theme::${theme}::setMenuColors {} ::ttk::awthemes::setMenuColors
    interp alias {} ::ttk::theme::${theme}::setTextColors {} ::ttk::awthemes::setTextColors
    interp alias {} ::ttk::theme::${theme}::hasImage {} ::ttk::awthemes::hasImage
    interp alias {} ::ttk::theme::${theme}::getColor {} ::ttk::awthemes::getColor
    interp alias {} ::ttk::theme::${theme}::getScaleFactor {} ::ttk::awthemes::getScaleFactor

    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::${theme} $var $var
    }

    set tkscale [tk scaling]
    if { $tkscale eq "Inf" || $tkscale eq "" } {
      tk scaling -displayof . 1.3333
      set tkscale 1.3333
    }
    set calcdpi [expr {round($tkscale*72.0)}]
    set vars(scale.factor) [expr {double($calcdpi)/100.0}]
    set colors(scale.factor) 1.0
    set vars(theme.name) $theme

    set iscript [info script]
    if { [file type $iscript] eq "link" } { set iscript [file link $iscript] }
    set d [file dirname $iscript]
    set vars(image.dir.awthemes) [file join $d i awthemes]
    set vars(cache.menu) [dict create]
    set vars(cache.text) [dict create]
    set vars(cache.listbox) [dict create]
    set vars(nb.img.width) 20
    set vars(nb.img.height) 3
    set vars(registered.combobox) [dict create]

    set vars(have.tksvg) false
    try {
      set ti [image create photo -data {<svg></svg>} -format svg]
      image delete $ti
      set vars(have.tksvg) true
    } on error {err res} {
      lassign [dict get $res -errorcode] a b c d
      if { $c ne "PHOTO_FORMAT" } {
        set vars(have.tksvg) true
      }
    }
    if { [info exists ::notksvg] && $::notksvg } {
      set vars(have.tksvg) false
    }

    # The rb/cb pad/small images are not listed here, as they
    # are dynamically generated.
    set vars(image.list) {
        arrow-down-d
        arrow-down-n
        arrow-left-d
        arrow-left-n
        arrow-right-d
        arrow-right-n
        arrow-up-d
        arrow-up-n
        button-a
        button-af
        button-d
        button-f
        button-n
        button-p
        cb-sa
        cb-sd
        cb-sn
        cb-ua
        cb-ud
        cb-un
        combo-arrow-down-a
        combo-arrow-down-d
        combo-arrow-down-n
        combo-button-a
        combo-button-f
        combo-button-d
        combo-button-n
        combo-entry-a
        combo-entry-d
        combo-entry-f
        combo-entry-n
        empty
        entry-a
        entry-d
        entry-f
        entry-n
        labelframe-d
        labelframe-n
        mb-arrow-down-a
        mb-arrow-down-d
        mb-arrow-down-n
        notebook-tab-a
        notebook-tab-d
        notebook-tab-h
        notebook-tab-i
        rb-sa
        rb-sd
        rb-sn
        rb-ua
        rb-ud
        rb-un
        sb-slider-ha
        sb-slider-hd
        sb-slider-h-grip
        sb-slider-hn
        sb-slider-hp
        sb-slider-va
        sb-slider-vd
        sb-slider-v-grip
        sb-slider-vn
        sb-slider-vp
        sb-trough-hd
        sb-trough-hn
        sb-trough-vd
        sb-trough-vn
        scale-ha
        scale-hf
        scale-hd
        scale-hn
        scale-hp
        scale-trough-hd
        scale-trough-hn
        scale-trough-vd
        scale-trough-vn
        scale-va
        scale-vd
        scale-vn
        scale-vp
        sizegrip
        slider-hd
        slider-hn
        slider-vd
        slider-vn
        spin-arrow-down-d
        spin-arrow-down-n
        spin-arrow-up-d
        spin-arrow-up-n
        tree-arrow-down-n
        tree-arrow-down-sn
        tree-arrow-right-n
        tree-arrow-right-sn
        trough-hn
        trough-hd
        trough-vn
        trough-vd
    }

    #
    #   image-name        fallback-name-list
    #
    # Order is important!!
    # A vertical may fall back to a horizontal in order to support
    # symmetrical handles.
    set vars(fallback.images) {
      arrow-down-d          arrow-down-n
      arrow-left-d          arrow-left-n
      arrow-right-d         arrow-right-n
      arrow-up-d            arrow-up-n
      arrow-down-sn         arrow-down-n
      arrow-right-sn        arrow-right-n
      mb-arrow-down-n       arrow-down-n
      mb-arrow-down-d       mb-arrow-down-n
      mb-arrow-down-a       mb-arrow-down-n
      combo-arrow-down-n    mb-arrow-down-n
      combo-arrow-down-a    combo-arrow-down-n
      combo-arrow-down-d    mb-arrow-down-d
      spin-arrow-down-d     arrow-down-d
      spin-arrow-down-n     arrow-down-n
      spin-arrow-up-d       arrow-up-d
      spin-arrow-up-n       arrow-up-n
      tree-arrow-right-n    arrow-right-n
      tree-arrow-right-sn   arrow-right-sn
      tree-arrow-down-n     arrow-down-n
      tree-arrow-down-sn    arrow-down-sn
      cb-sa                 cb-sn
      cb-sd                 cb-sn
      cb-sn-pad             cb-sn
      cb-sn-small           cb-sn
      cb-ua                 cb-un
      cb-ud                 cb-un
      cb-un-pad             cb-un
      cb-un-small           cb-un
      rb-sa                 rb-sn
      rb-sd                 rb-sn
      rb-sn-pad             rb-sn
      rb-sn-small           rb-sn
      rb-ua                 rb-un
      rb-ud                 rb-un
      rb-un-pad             rb-un
      rb-un-small           rb-un
      scale-hf              scale-hn
      scale-ha              scale-hf
      scale-hd              scale-hn
      scale-hp              scale-ha
      # { a scale-vf falls back to a scale-vn preferentially }
      # { over a scale-ha.   The scale-vn must do its fallback }
      # { to a scale-hn afterwards so that the scale-vf will }
      # { pick up the scale-ha image if there is no scale-vn. }
      scale-vf              {scale-vn scale-ha}
      scale-va              scale-vf
      scale-vn              scale-hn
      scale-vd              scale-vn
      scale-vp              scale-va
      slider-hd             slider-hn
      slider-vd             slider-vn
      button-d              button-n
      button-f              button-n
      button-a              button-f
      button-af             button-a
      button-p              button-af
      entry-f               entry-n
      entry-a               entry-f
      entry-d               entry-n
      combo-entry-n         entry-n
      combo-entry-d         entry-d
      combo-entry-f         entry-f
      combo-entry-a         entry-a
      combo-button-a        combo-entry-a
      combo-button-f        combo-entry-f
      combo-button-d        combo-entry-d
      combo-button-n        combo-entry-n
      sb-slider-hn          slider-hn
      sb-slider-hd          slider-hd
      sb-slider-ha          sb-slider-hn
      sb-slider-hp          sb-slider-ha
      sb-slider-vn          slider-vn
      sb-slider-vd          slider-vd
      sb-slider-va          sb-slider-vn
      sb-slider-vp          sb-slider-va
      notebook-tab-a        notebook-tab-i
      notebook-tab-h        notebook-tab-a
      notebook-tab-d        notebook-tab-i
      trough-hd             trough-hn
      trough-vd             trough-vn
      scale-trough-hn       trough-hn
      scale-trough-hd       trough-hd
      scale-trough-vn       trough-vn
      scale-trough-vd       trough-vd
      sb-trough-hd          trough-hd
      sb-trough-hn          trough-hn
      sb-trough-vd          trough-vd
      sb-trough-vn          trough-vn
      labelframe-d          labelframe-n
    }

    _initializeColors $theme

    # only need to do this for the requested theme
    set starttime [clock milliseconds]
    _loadImageData $theme $vars(image.dir.awthemes)
    _copyDerivedImageData $theme
    _setImageData $theme
    set endtime [clock milliseconds]
    if { $::ttk::awthemes::debug > 0 } {
      puts "t:   image setup: [expr {$endtime-$starttime}]"
    }

    set starttime [clock milliseconds]
    _createTheme $theme
    initOptiondb $theme
    set endtime [clock milliseconds]
    if { $::ttk::awthemes::debug > 0 } {
      puts "t:   theme creation: [expr {$endtime-$starttime}]"
    }
    set gendtime [clock milliseconds]
    if { $::ttk::awthemes::debug > 0 } {
      puts "t: init: [expr {$gendtime-$gstarttime}]"
    }
  }

  proc initOptiondb { theme } {
    # ttk::style theme use has not been run yet, so the
    # theme must be supplied.
    if { $::tcl_platform(platform) ne "windows" } {
      setMenuColors -optiondb $theme
    }
    setListboxColors -optiondb 0 $theme
    setTextColors -optiondb -entry $theme
  }

  proc _initializeColors { theme } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::${theme} $var $var
    }

    set starttime [clock milliseconds]

    # default styles.
    # the theme will override the style with the theme specific styles.
    foreach {st sn} $::themeutils::vars(names.styles) {
      set colors(style.$st) $sn
    }

    _setThemeBaseColors $theme
    if { ! [info exists colors(is.dark)] } {
      set colors(is.dark) false
    }

    set colors(bg.bg.latest) $colors(bg.bg)
    set colors(graphics.color.latest) $colors(graphics.color)

    # override styles with user defined styles.
    foreach {st sn} $::themeutils::vars(names.styles) {
      if { [info exists colors(user.style.$st)] } {
        set colors(style.$st) $colors(user.style.$st)
        if { $::ttk::awthemes::debug > 0 } {
          puts "st: user.style: $st $colors(user.style.$st)"
        }
      }
    }

    # 9.4.2 fix a design flaw.  The settings.tcl file for each
    # widget style should be loaded before the user colors are
    # loaded and before the derived colors are processed.
    # This will help simplify some settings.
    _loadImageData $theme $vars(image.dir.awthemes) -settingsonly

    # override base colors with any user.* colors
    # these are set by the ::themeutils package
    # process the base colors first, then the derived colors
    foreach {n} $::themeutils::vars(names.colors.base) {
      if { [info exists colors(user.$n)] } {
        set colors($n) $colors(user.$n)
        if { $::ttk::awthemes::debug > 0 } {
          puts "c: user: $n $colors(user.$n)"
        }
      }
    }

    _setDerivedColors $theme

    # now override any derived colors with user-specified colors
    foreach {grp n val type} $::themeutils::vars(names.colors.derived) {
      if { [info exists colors(user.$n)] } {
        set colors($n) $colors(user.$n)
        if { $::ttk::awthemes::debug > 0 } {
          puts "c: user: $n $colors(user.$n)"
        }
      }
    }

    set colors(bg.bg.latest) $colors(bg.bg)
    set colors(graphics.color.latest) $colors(graphics.color)
    set endtime [clock milliseconds]
    if { $::ttk::awthemes::debug > 0 } {
      puts "t:   color setup: [expr {$endtime-$starttime}]"
    }
  }

  proc _setThemeBaseColors { theme } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::${theme} $var $var
    }

    ::ttk::theme::${theme}::setBaseColors
  }

  proc _processDerivedColors { theme l } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::${theme} $var $var
    }

    # run through the list multiple times so that derived colors based
    # on other derived colors get set.
    set ll [llength $l]
    set nl {}
    set count 0
    while { $ll > 0 && $count < 4} {
      if { $::ttk::awthemes::debug > 0 } {
        puts "c: == pass $count"
      }
      if { $ll % 4 != 0 } {
        error "bad list : $count"
      }
      foreach {grp n value type} $l {
        if { [info exists colors($n)] } {
          # skip any color that is already set
          if { $::ttk::awthemes::debug > 0 } {
            puts "c: already: $n $colors($n)"
          }
          continue
        } elseif { $type eq "base" } {
          # base colors are not changed
          continue
        } elseif { $type eq "static" } {
          set colors($n) $value
          if { $::ttk::awthemes::debug > 0 } {
            puts "c: static: $n $colors($n)"
          }
        } elseif { $type eq "color" } {
          if { ! [info exists colors($value)] } {
            lappend nl $grp $n $value $type
          } else {
            set colors($n) $colors($value)
            if { $::ttk::awthemes::debug > 0 } {
              puts "c: derived: $n ($value) $colors($value)"
            }
          }
        } elseif { $type eq "disabled" ||
            $type eq "highlight" ||
            $type eq "black" ||
            $type eq "white" } {
          lassign $value col perc

          if { ! [info exists colors($col)] } {
            lappend nl $grp $n $value $type
          } else {
            set proc ::colorutils::opaqueBlendPerc

            if { $type eq "highlight" } {
              set type black
              if { $colors(is.dark) } {
                set type white
              }
            }
            if { $type eq "disabled" } {
              set blend $colors(bg.bg)
              set proc ::colorutils::disabledColor
              if { $colors($col) eq "#000000" } {
                set blend #ffffff
                set perc 0.6
                if { $colors(is.dark) } {
                  set perc 0.85
                }
              }
              if { $colors($col) eq "#ffffff" } {
                set blend #000000
                set perc 0.94
                if { $colors(is.dark) } {
                  set perc 0.7
                }
              }
            } elseif { $type eq "black" } {
              set blend #000000
            } elseif { $type eq "white" } {
              set blend #ffffff
            }
            set colors($n) [$proc $colors($col) $blend $perc 2]
            if { $::ttk::awthemes::debug > 0 } {
              puts "c: compute: $n $colors($n)"
            }
          }
        }
      }
      set l $nl
      set ll [llength $nl]
      set nl {}
      incr count
    }
    if { $count >= 4 } {
      puts stderr "==== $count"
      foreach {grp n value type} $l {
        if { ! [info exists colors($n)] } {
          puts stderr "$n /$value/ $type"
        }
      }
      puts stderr "===="
      error "invalid setup, could not derive colors"
    }
  }

  proc _setDerivedColors { theme } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::${theme} $var $var
    }

    # set up the basic derived colors
    set l $::themeutils::vars(names.colors.derived.basic)
    _processDerivedColors $theme $l

    # set the theme specific colors
    ::ttk::theme::${theme}::setDerivedColors

    # now calculate the rest of the derived colors
    # the colors set by the theme will be skipped.
    set l $::themeutils::vars(names.colors.derived)
    _processDerivedColors $theme $l
  }

  proc _readFile { fn } {
    set fh [open $fn]
    set fs [file size $fn]
    set data [read $fh $fs]
    close $fh
    return $data
  }

  proc _copyDerivedImageData { theme } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$theme $var $var
    }
    namespace upvar ::ttk::theme::$theme imgtype imgtype
    namespace upvar ::ttk::theme::$theme rawimgdata rawimgdata

    if { ! $vars(have.tksvg) } {
      return
    }

    # copy derived image files
    # if a theme has an image, it should be used for all possible
    # images rather than using the generics.
    if { [llength $vars(fallback.images)] % 2 != 0 } {
      error "bad fallback definition"
    }
    dict for {n fb} $vars(fallback.images) {
      if { $n eq {#} } {
        continue
      }
      if { $n eq $fb } {
        puts stderr "bad fallback config: $n $fb"
        continue
      }
      if { [info exists rawimgdata($n)] } {
        continue
      }
      if { ! [info exists rawimgdata($n)] &&
          [dict exists $vars(fallback.images) $n] } {
        foreach {ttn} [dict get $vars(fallback.images) $n] {
          if { $ttn ne $n && [info exists rawimgdata($ttn)] } {
            if { $::ttk::awthemes::debug > 0 } {
              puts "i: fallback: $n $ttn"
            }
            set rawimgdata($n) $rawimgdata($ttn)
            set imgtype($n) $imgtype($ttn)
            dict set vars(images) $n 1
            break
          }
        }
      } ; # if the image has not been loaded
    } ; # for each fallback image
  }

  proc _loadImageData { theme basedir {settingsonly {}} } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$theme $var $var
    }
    namespace upvar ::ttk::theme::$theme imgtype imgtype
    namespace upvar ::ttk::theme::$theme rawimgdata rawimgdata

    if { ! $vars(have.tksvg) } {
      return
    }

    if { ! [file isdirectory $basedir] } {
      return
    }

    foreach {st sn} $::themeutils::vars(names.styles) {
      # if the override is for no graphics, skip this style type
      if { $colors(style.$st) eq "none" } {
        continue
      }
      if { $colors(style.$st) eq "-" || $colors(style.$st) eq "default" } {
        continue
      }

      # Check and see if the style type directory and the specific style
      # directory exist.  If not, skip.
      set styletypedir [file join $basedir $st]
      if { $st eq "scrollbar" } {
        # fetch scrollbar slider styles from the progressbar dir.
        set styletypedir [file join $basedir progressbar]
      }
      if { ! [file isdirectory $styletypedir] } {
        puts stderr "error: unable to locate widget type: $st"
        continue
      }
      set styledir [file join $styletypedir $colors(style.$st)]
      if { ! [file isdirectory $styledir] } {
        puts stderr "error: unable to locate widget style: $colors(style.$st)"
        set colors(style.$st) none
        continue
      }

      if { $::ttk::awthemes::debug > 0 } {
        puts "i: style: $styledir"
      }

      if { $settingsonly eq {} } {
        foreach {fn} [glob -directory $styledir *.svg] {
          if { [string match *-base* $fn] } { continue }
          set origi [file rootname [file tail $fn]]
          if { $st eq "scrollbar" } {
            set origi "sb-${origi}"
          }
          if { ! [info exists imgtype($origi)] } {
            set imgtype($origi) svg
            set rawimgdata($origi) [_readFile $fn]
            dict set vars(images) $origi 1
            if { $::ttk::awthemes::debug > 0 } {
              puts "i: loaded: $origi for $st"
            }
          }
        }
      }

      # settings were already loaded, no need to load them again
      if { $settingsonly ne {} } {
        set fn [file join $styledir settings.tcl]
        if { [file exists $fn] } {
          try {
            if { $::ttk::awthemes::debug > 0 } {
              puts "i: settings from $styledir"
            }
            source $fn
          } on error {err res} {
            puts stderr "load $fn: $res"
          }
        }
      } ; # settings only
    } ; # foreach widget style
  }

  proc _setImageData { theme {sfx {}} } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$theme $var $var
    }
    namespace upvar ::ttk::theme::$theme imgtype imgtype
    namespace upvar ::ttk::theme::$theme rawimgdata rawimgdata

    if { $vars(have.tksvg) } {
      # convert all the svg colors to theme specific colors
      # this mess could be turned into some sort of table driven method.

      # make a copy of the raw image data before modifying it.
      foreach {n} [dict keys $vars(images)] {
        set imgdata($n$sfx) $rawimgdata($n)
        set imgtype($n$sfx) $imgtype($n)
      }

      # scrollbar pressed color
      foreach {n} {
          sb-slider-hp sb-slider-vp
          } {
        foreach {oc nc} [list \
            _GC_      $colors(scrollbar.color.pressed) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # button pressed color
      foreach {n} {
          button-p
          } {
        foreach {oc nc} [list \
            _BUTTONBG_    $colors(button.pressed) \
            _BUTTONBORD_  $colors(border.button.active) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # button active-focus color
      foreach {n} {
          button-af
          } {
        foreach {oc nc} [list \
            _BUTTONBG_    $colors(button.active.focus) \
            _BUTTONBORD_  $colors(border.button.active) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # button active color
      foreach {n} {
          button-a combo-button-a
          } {
        foreach {oc nc} [list \
            _BUTTONBG_    $colors(button.active) \
            _BUTTONBORD_  $colors(border.button.active) \
            _FIELDBG_     $colors(button.active) \
            _BORD_        $colors(border.button.active) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # button normal color
      foreach {n} {
          combo-button-n combo-button-d
          } {
        foreach {oc nc} [list \
            _FIELDBG_   $colors(button) \
            _BORD_      $colors(border.button) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # active scrollbar color
      foreach {n} {
          sb-slider-ha sb-slider-va
          } {
        foreach {oc nc} [list \
            _GC_      $colors(scrollbar.color.active) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # scrollbar trough color
      foreach {n} {
          sb-trough-hd sb-trough-hn sb-trough-vd sb-trough-vn
          } {
        foreach {oc nc} [list \
            _TROUGH_      $colors(scrollbar.trough) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # in some themes the scrollbar has a different color
      foreach {n} {sb-slider-h-grip sb-slider-v-grip
          sb-slider-ha sb-slider-hd sb-slider-hn sb-slider-hp
          sb-slider-va sb-slider-vd sb-slider-vn sb-slider-vp
          } {
        foreach {oc nc} [list \
            _GC_      $colors(scrollbar.color) \
            _GCBORD_    $colors(scrollbar.border) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # in some themes the scale has a different color
      foreach {n} {
          scale-ha scale-hf scale-hd scale-hn scale-hp
          scale-va scale-vf scale-vd scale-vn scale-vp
          } {
        foreach {oc nc} [list \
            _GC_        $colors(scale.color) \
            _GCBORD_    $colors(border.scale) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # the scale trough in some themes has a different color
      foreach {n} {
          scale-trough-hn scale-trough-hd scale-trough-vn scale-trough-vd
          } {
        foreach {oc nc} [list \
            _TROUGH_    $colors(scale.trough) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # the progressbar may be colored differently.
      foreach {n} {slider-hd slider-hn slider-vd slider-vn} {
        foreach {oc nc} [list \
            _GC_    $colors(pbar.color) \
            _GCBORD_  $colors(pbar.color.border) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # scrollbar arrow colors
      # _GCARR_ : the solid-bg arrow theme uses this as the arrow color.
      # _GC_    : other themes use this as the arrow color.
      # _GCBG_  : the solid-bg arrow theme uses this as the background.
      foreach {n} {
          arrow-left-d arrow-left-n
          arrow-down-d arrow-down-n
          arrow-right-d arrow-right-n
          arrow-up-d arrow-up-n
          } {
        foreach {oc nc} [list \
            _GCBG_  $colors(spinbox.color.bg) \
            _GCARR_ $colors(scrollbar.color.arrow) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # selected treeview arrow colors
      foreach {n} {
          tree-arrow-down-sn tree-arrow-right-sn
          } {
        foreach {oc nc} [list \
            _GCARR_ $colors(tree.arrow.selected) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # spinbox/menubutton arrow colors
      # _GCARR_ : the solid-bg arrow theme uses this as the arrow color
      # _GC_ : used by the all other arrow themes.
      foreach {n} {spin-arrow-down-d spin-arrow-down-n
          spin-arrow-up-d spin-arrow-up-n
          mb-arrow-down-a mb-arrow-down-d mb-arrow-down-n
          } {
        foreach {oc nc} [list \
            _GC_      $colors(spinbox.color.arrow) \
            _GCARR_   $colors(spinbox.color.arrow) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      # combobox arrow colors
      # _GCARR_ : the solid-bg arrow theme uses this as the arrow color
      # _GC_ : used by the all other arrow themes.
      foreach {n} {
          combo-arrow-down-a combo-arrow-down-d combo-arrow-down-n
          } {
        foreach {oc nc} [list \
            _GC_      $colors(combobox.color.arrow) \
            _GCARR_   $colors(combobox.color.arrow) \
            ] {
          if { [info exists imgdata($n$sfx)] } {
            set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
          }
        }
      }

      foreach {n} [dict keys $vars(images)] {
        # special handling for radio buttons so that the width
        # can be set for the padded version.
        if { [regexp {^[rc]b-} $n] } {
          regexp {\sheight="(\d+)"} $imgdata($n$sfx) all value
          if { [regexp {pad} $n] } {
            set value [expr {round(double($value)*1.4)}]
          }
          regsub -all _MAINWIDTH_ $imgdata($n$sfx) $value imgdata($n$sfx)
        }
        foreach {oc nc} [list \
            _ACTIVE_    $colors(active.color) \
            _ACCENT_    $colors(accent.color) \
            _BG_        $colors(bg.bg) \
            _BORD_      $colors(border) \
            _BUTTONBG_  $colors(button) \
            _BUTTONBORD_ $colors(border.button) \
            _CBBG_      $colors(entrybg.checkbutton) \
            _CBBORD_    $colors(border.checkbutton) \
            _DARK_      $colors(bg.dark) \
            _FG_        $colors(fg.fg) \
            _FIELDBG_   $colors(entrybg.bg) \
            _FOCUS_     $colors(focus.color) \
            _GC_        $colors(graphics.color) \
            _GCBG_      $colors(spinbox.color.bg) \
            _GCLIGHT_   $colors(graphics.color.light) \
            _GCARR_     $colors(arrow.color) \
            _GCHL_      $colors(graphics.highlight) \
            _GRIP_      $colors(scrollbar.color.grip) \
            _LIGHT_     $colors(bg.lighter) \
            _SZGRIP_    $colors(sizegrip.color) \
            _TROUGH_    $colors(trough.color) \
            ] {
          set c [regsub -all $oc $imgdata($n$sfx) $nc imgdata($n$sfx)]
        }
      }
    }

      # if using tksvg, this block may be removed to make the code smaller.
      # removed
  }

  # the image suffix has already been applied to the name
  proc _mkimage { n {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }
    namespace upvar ::ttk::theme::$currtheme imgtype imgtype

    if { $vars(have.tksvg) &&
        [info exists imgtype($n)] &&
        $imgtype($n) eq "svg" &&
        [info exists imgdata($n)] } {
      set sf [expr {$vars(scale.factor)*$colors(scale.factor)*$scale}]
      if { $::ttk::awthemes::debug > 0 } {
        puts "i: mk svg $n"
      }
      try {
        set images($n) [image create photo -data $imgdata($n) \
            -format "svg -scale $sf"]
      } on error {err res} {
        puts stderr "failed to make image: $n"
        puts stderr $res
        exit 1
      }
    }
    if { ! [info exists images($n)] && [info exists imgdata($n)] } {
      if { $::ttk::awthemes::debug > 0 } {
        puts "i: mk png $n"
      }
      set images($n) [image create photo -data $imgdata($n)]
    }
  }

  proc _adjustSizes { nm scale} {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    set sf [expr {$vars(scale.factor)*$colors(scale.factor)*$scale}]
    set vals {}
    foreach {sz} $colors($nm) {
      lappend vals [expr {round(double($sz)*$sf)}]
    }
    return $vals
  }

  proc _createButton { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(button-n)] } {

      # adjust the borders and padding by the scale factor
      set imgbord [_adjustSizes button.image.border $scale]
      set imgpad [_adjustSizes button.image.padding $scale]

      ::ttk::style element create ${pfx}Button.button image \
          [list $images(button-n${sfx}) \
          disabled $images(button-d${sfx}) \
          {active focus !pressed !disabled} $images(button-af${sfx}) \
          {focus !pressed !disabled} $images(button-f${sfx}) \
          {active !pressed !disabled} $images(button-a${sfx}) \
          {pressed !disabled} $images(button-p${sfx})] \
          -sticky nsew \
          -border $imgbord \
          -padding $imgpad

      if { $colors(button.has.focus) } {
        ::ttk::style layout ${pfx}TButton [list \
          ${pfx}Button.button -children [list \
            Button.focus -children [list \
              Button.padding -children [list \
                Button.label -expand true \
              ] \
            ] \
          ] \
        ]
      } else {
        ::ttk::style layout ${pfx}TButton [list \
          ${pfx}Button.button -children [list \
            Button.padding -children [list \
              Button.label -expand true \
            ] \
          ] \
        ]
      }
    }

    ::ttk::style configure ${pfx}TButton \
        -width -8 \
        -borderwidth 1 \
        -relief $colors(button.relief)
  }

  # prefix must include the trailing dot.
  proc _createCheckButton { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    # the non-tksvg awdark and awlight themes define cb-un
    if { [info exists images(cb-un)] } {
      ::ttk::style element create ${pfx}Checkbutton.indicator image \
          [list $images(cb-un${sfx}) \
          {hover selected !disabled} $images(cb-sa${sfx}) \
          {hover !selected !disabled} $images(cb-ua${sfx}) \
          {!selected disabled} $images(cb-ud${sfx}) \
          {selected disabled} $images(cb-sd${sfx}) \
          {selected !disabled} $images(cb-sn${sfx})] \
          -padding 0

      # the new layout puts the focus around both the checkbutton image
      # and the label.
      ::ttk::style layout ${pfx}TCheckbutton [list \
        Checkbutton.focus -side left -sticky w -children [list \
          ${pfx}Checkbutton.indicator -side left -sticky {} \
          Checkbutton.padding -children [list \
            Checkbutton.label -sticky nsew \
          ]
        ]
      ]
    }

    ::ttk::style configure ${pfx}TCheckbutton \
        -borderwidth 0 \
        -relief none

    if { [info exists images(cb-un-small)] } {
      ::ttk::style element create ${pfx}Menu.Checkbutton.indicator image \
          [list $images(cb-un-small${sfx}) \
          {selected !disabled} $images(cb-sn-small${sfx})] \
    }

    ::ttk::style layout ${pfx}Flexmenu.TCheckbutton [list \
      Checkbutton.padding -sticky nswe -children [list \
        ${pfx}Menu.Checkbutton.indicator -side left -sticky {} \
      ] \
    ]

    ::ttk::style configure ${pfx}Flexmenu.TCheckbutton \
        -borderwidth 0 \
        -relief none \
        -focusthickness 0 \
        -indicatormargin 0
  }

  proc _createCombobox { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(combo-arrow-down-n)] } {

      # using -sticky ns messes up combobox down arrows that are
      # not using the full height.
      set imgbord $colors(combobox.image.border)
      set sticky $colors(combobox.image.sticky)

      ::ttk::style element create ${pfx}Combobox.downarrow image \
          [list $images(combo-arrow-down-n${sfx}) \
          {active !disabled} $images(combo-arrow-down-a${sfx}) \
          disabled $images(combo-arrow-down-d${sfx}) \
          ] \
          -sticky $sticky \
          -border $imgbord

      if { $vars(have.tksvg) &&
          [info exists images(combo-entry-n)] &&
          [info exists images(combo-button-n)] } {
        # if there are entry images and button images, set up the
        # combobox as an entry and a readonly combobox as a button
        set imgbord [_adjustSizes combobox.entry.image.border $scale]
        set imgpad [_adjustSizes combobox.entry.image.padding $scale]

        ::ttk::style element create ${pfx}Combobox.field \
            image [list $images(combo-entry-n${sfx}) \
            {readonly disabled}  $images(combo-button-d${sfx}) \
            {!readonly disabled} $images(combo-entry-d${sfx}) \
            {focus readonly !disabled} $images(combo-button-f${sfx}) \
            {hover readonly !disabled} $images(combo-button-a${sfx}) \
            {focus !readonly !disabled} $images(combo-entry-f${sfx}) \
            {hover !readonly !disabled} $images(combo-entry-a${sfx}) \
            {readonly !disabled}  $images(combo-button-n${sfx}) \
            ] \
            -border $imgbord \
            -padding $imgpad \
            -sticky nsew
      } elseif { $vars(have.tksvg) &&
          [info exists images(combo-entry-n)] } {
        # if there are entry images set up the combobox as an entry.
        set imgbord [_adjustSizes combobox.entry.image.border $scale]
        set imgpad [_adjustSizes combobox.entry.image.padding $scale]

        ::ttk::style element create ${pfx}Combobox.field \
            image [list $images(combo-entry-n${sfx}) \
            disabled $images(combo-entry-d${sfx}) \
            {focus !disabled} $images(combo-entry-f${sfx}) \
            {hover !disabled} $images(combo-entry-a${sfx}) \
            ] \
            -border $imgbord \
            -padding $imgpad \
            -sticky nsew
      }

      if { $pfx ne {} } {
        set layout [::ttk::style layout TCombobox]
        regsub {(Combobox.downarrow)} $layout "${pfx}\\1" layout
        ::ttk::style layout ${pfx}TCombobox $layout
      }
      dict set vars(registered.combobox) ${pfx}TCombobox $pfx
    }

    ::ttk::style configure ${pfx}TCombobox \
        -borderwidth 1 \
        -relief none
  }

  proc _createEntry { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(entry-n)] } {
      # adjust the borders and padding by the scale factor
      set imgbord [_adjustSizes entry.image.border $scale]
      set imgpad [_adjustSizes entry.image.padding $scale]

      ::ttk::style element create ${pfx}Entry.field image \
          [list $images(entry-n${sfx}) \
          disabled $images(entry-d${sfx}) \
          {focus !disabled} $images(entry-f${sfx}) \
          {hover !disabled} $images(entry-a${sfx}) \
          ] \
          -sticky nsew \
          -border $imgbord \
          -padding $imgpad \
    }

    ::ttk::style configure ${pfx}TEntry \
        -borderwidth 0 \
        -relief none
  }

  proc _createLabelframe { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(labelframe-n)] } {
      ::ttk::style element create ${pfx}Labelframe.border image \
          [list $images(labelframe-n) \
          disabled $images(labelframe-d)] \
          -border 4 \
          -sticky news
    }

    # labelframe borders cannot change color with the 'default' theme.
    # set this up to work with 'clam' based themes.
    ::ttk::style configure ${pfx}TLabelframe \
        -borderwidth 1 \
        -relief groove
  }

  proc _createMenubutton { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(mb-arrow-down-n)] } {
      ::ttk::style element create ${pfx}Menubutton.indicator image \
          [list $images(mb-arrow-down-n${sfx}) \
          disabled $images(mb-arrow-down-d${sfx}) \
          {active !disabled} $images(mb-arrow-down-a${sfx}) \
          ]

      if { $colors(menubutton.use.button.image) &&
          [info exists images(button-n)] } {
        # adjust the borders and padding by the scale factor
        set imgbord [_adjustSizes button.image.border $scale]
        set imgpad [_adjustSizes button.image.padding $scale]

        # menubuttons have no focus (so why do they have a focus ring?)
        # the original breeze style uses different images for
        # a menubutton than a button.  This may need to be changed
        # to be dynamic if another style does the same thing for
        # menubuttons.
        ::ttk::style element create ${pfx}Menubutton.border image \
            [list $images(button-n${sfx}) \
            disabled $images(button-n${sfx}) \
            {active !pressed !disabled} $images(button-a${sfx}) \
            {pressed !disabled} $images(button-p${sfx})] \
            -sticky nsew \
            -border $imgbord \
            -padding $imgpad
      }

      if { $pfx ne {} } {
        set layout [::ttk::style layout TMenubutton]
        regsub {(Menubutton.indicator)} $layout "${pfx}\\1" layout
        ::ttk::style layout ${pfx}TMenubutton $layout
      }
    }

    ::ttk::style configure ${pfx}TMenubutton \
        -borderwidth 1
  }

  proc _createNotebook { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    _createNotebookStyle $currtheme $pfx

    if { $vars(have.tksvg) && [info exists images(notebook-tab-i)] } {
      # adjust the borders and padding by the scale factor
      set imgbord [_adjustSizes tab.image.border $scale]
      set imgpad [_adjustSizes tab.image.padding $scale]

      ::ttk::style element create ${pfx}tab image \
          [list $images(notebook-tab-i${sfx}) \
          {selected !disabled} $images(notebook-tab-a${sfx}) \
          {active !selected !disabled} $images(notebook-tab-h${sfx}) \
          disabled $images(notebook-tab-d${sfx})] \
          -border $imgbord \
          -padding $imgpad
    }

    ::ttk::style configure ${pfx}TNotebook \
        -borderwidth 0
    # themes with 'default' as parent would require a borderwidth to be set.
    # though the 'default' tabs are nice, the border colors are not.
    ::ttk::style configure ${pfx}TNotebook.Tab \
        -borderwidth $colors(notebook.tab.borderwidth)
    ::ttk::style map ${pfx}TNotebook.Tab \
        -borderwidth [list disabled 0]
  }

  proc _createPanedwindow { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme

    # for some themes, it is possible to configure a sash handle image.

    ::ttk::style configure ${pfx}Sash \
        -sashthickness 10
  }

  proc _createProgressbar { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(slider-hn)] } {
      # adjust the borders and padding by the scale factor
      set imgbord [_adjustSizes slider.image.border $scale]
      set imgpad [_adjustSizes slider.image.padding $scale]

      ::ttk::style element create ${pfx}Horizontal.Progressbar.pbar image \
          [list $images(slider-hn${sfx}) \
          disabled $images(slider-hd${sfx})] \
          -border $imgbord \
          -padding $imgpad
      set imgbord [list [lindex $imgbord 1] [lindex $imgbord 0]]
      ::ttk::style element create ${pfx}Vertical.Progressbar.pbar image \
          [list $images(slider-vn${sfx}) \
          disabled $images(slider-vd${sfx})] \
          -border $imgbord \
          -padding $imgpad
      if { [info exists images(trough-hn)] } {
        set imgbord [_adjustSizes trough.image.border $scale]
        set imgpad [_adjustSizes trough.image.padding $scale]
        ::ttk::style element create ${pfx}Horizontal.Progressbar.trough image \
            [list $images(trough-hn${sfx}) \
            disabled $images(trough-hd${sfx})] \
            -border $imgbord \
            -padding $imgpad
      }
      if { [info exists images(trough-vn)] } {
        set imgbord [list [lindex $imgbord 1] [lindex $imgbord 0]]
        ::ttk::style element create ${pfx}Vertical.Progressbar.trough image \
            [list $images(trough-vn${sfx}) \
            disabled $images(trough-vd${sfx})] \
            -border $imgbord \
            -padding $imgpad
      }

      if { $pfx ne {} } {
        set layout [::ttk::style layout Horizontal.TProgressbar]
        regsub {(Horizontal.Progressbar.pbar)} $layout "${pfx}\\1" layout
        if { [info exists images(trough-hn)] } {
          regsub {(Horizontal.Progressbar.trough)} $layout "${pfx}\\1" layout
        }
        ::ttk::style layout ${pfx}Horizontal.TProgressbar $layout

        set layout [::ttk::style layout Vertical.TProgressbar]
        regsub {(Vertical.Progressbar.pbar)} $layout "${pfx}\\1" layout
        if { [info exists images(trough-vn)] } {
          regsub {(Vertical.Progressbar.trough)} $layout "${pfx}\\1" layout
        }
        ::ttk::style layout ${pfx}Vertical.TProgressbar $layout
      }
    }

    set layout [::ttk::style layout ${pfx}Horizontal.TProgressbar]
    regsub -- {-sticky nswe} $layout "-sticky we" layout
    regsub -- {-sticky ns} $layout "-sticky {}" layout
    ::ttk::style layout ${pfx}Horizontal.TProgressbar $layout

    set layout [::ttk::style layout ${pfx}Vertical.TProgressbar]
    regsub -- {-sticky nswe} $layout "-sticky ns" layout
    regsub -- {-sticky we} $layout "-sticky {}" layout
    ::ttk::style layout ${pfx}Vertical.TProgressbar $layout

    ::ttk::style configure ${pfx}TProgressbar \
        -borderwidth 0 \
        -pbarrelief none
  }

  # prefix must include the trailing dot.
  proc _createRadioButton { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { [info exists images(rb-un)] } {
      ::ttk::style element create ${pfx}Radiobutton.indicator image \
          [list $images(rb-un$sfx) \
          {hover selected !disabled} $images(rb-sa$sfx) \
          {hover !selected !disabled} $images(rb-ua$sfx) \
          {!selected disabled} $images(rb-ud$sfx) \
          {selected disabled} $images(rb-sd$sfx) \
          {selected !disabled} $images(rb-sn$sfx)] \
          -padding 0
    }

    ::ttk::style layout ${pfx}TRadiobutton [list \
      Radiobutton.focus -side left -sticky w -children [list \
        ${pfx}Radiobutton.indicator -side left -sticky {} \
        Radiobutton.padding -sticky nswe -children { \
          Radiobutton.label -sticky nswe \
        } \
      ] \
    ]

    ::ttk::style configure ${pfx}TRadiobutton \
        -borderwidth 0 \
        -relief none

    if { [info exists images(rb-un-small)] } {
      ::ttk::style element create ${pfx}Menu.Radiobutton.indicator image \
          [list $images(rb-un-small${sfx}) \
          {selected} $images(rb-sn-small${sfx})]
    }

    ::ttk::style layout ${pfx}Flexmenu.TRadiobutton [list \
      Radiobutton.padding -sticky nswe -children [list \
        ${pfx}Menu.Radiobutton.indicator -side left -sticky {} \
      ] \
    ]

    ::ttk::style configure ${pfx}Flexmenu.TRadiobutton \
        -borderwidth 0 \
        -relief none \
        -focusthickness 0 \
        -indicatormargin 0
  }

  proc _createScale { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(scale-hn)] } {
      # adjust the borders and padding by the scale factor
      set imgbord [_adjustSizes scale.slider.image.border $scale]

      ::ttk::style element create ${pfx}Horizontal.Scale.slider image \
          [list $images(scale-hn${sfx}) \
          disabled $images(scale-hd${sfx}) \
          {pressed !disabled} $images(scale-hp${sfx}) \
          {active !pressed !disabled} $images(scale-ha${sfx}) \
          ] \
          -sticky {}
      ::ttk::style element create ${pfx}Vertical.Scale.slider image \
          [list $images(scale-vn${sfx}) \
          disabled $images(scale-vd${sfx}) \
          {pressed !disabled} $images(scale-vp${sfx}) \
          {active !pressed !disabled} $images(scale-va${sfx}) \
          ] \
          -sticky {}

      if { [info exists images(scale-trough-hn)] } {
        set imgbord [_adjustSizes scale.trough.image.border $scale]
        ::ttk::style element create ${pfx}Horizontal.Scale.trough image \
            [list $images(scale-trough-hn${sfx}) \
            disabled $images(scale-trough-hd${sfx}) \
            ] \
            -border $imgbord \
            -padding 0 \
            -sticky ew
      }
      if { [info exists images(scale-trough-vn)] } {
        set imgbord [list [lindex $imgbord 1] [lindex $imgbord 0]]
        ::ttk::style element create ${pfx}Vertical.Scale.trough image \
            [list $images(scale-trough-vn${sfx}) \
            disabled $images(scale-trough-vd${sfx}) \
            ] \
            -border $imgbord \
            -padding 0 \
            -sticky ns
      }

      if { $pfx ne {} } {
        set layout [::ttk::style layout Horizontal.TScale]
        regsub {(Horizontal.Scale.slider)} $layout "${pfx}\\1" layout
        if { [info exists images(scale-trough-hn)] } {
          regsub {(Horizontal.Scale.trough)} $layout "${pfx}\\1" layout
        }
        ::ttk::style layout ${pfx}Horizontal.TScale $layout

        set layout [::ttk::style layout Vertical.TScale]
        regsub {(Vertical.Scale.slider)} $layout "${pfx}\\1" layout
        if { [info exists images(scale-trough-vn)] } {
          regsub {(Vertical.Scale.trough)} $layout "${pfx}\\1" layout
        }
        ::ttk::style layout ${pfx}Vertical.TScale $layout
      }
    }

    ::ttk::style configure ${pfx}TScale \
        -borderwidth 1
  }

  proc _createScrollbars { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(slider-hn)] } {
      set imgbord [_adjustSizes slider.image.border $scale]
      set imgpad [_adjustSizes slider.image.border $scale]

      if { $colors(scrollbar.has.arrows) } {
        ::ttk::style element create ${pfx}Horizontal.Scrollbar.leftarrow image \
            [list $images(arrow-left-n${sfx}) \
            disabled  $images(arrow-left-d${sfx})]
        ::ttk::style element create ${pfx}Horizontal.Scrollbar.rightarrow image \
            [list $images(arrow-right-n${sfx}) \
            disabled  $images(arrow-right-d${sfx})]
      }
      set hasgrip false
      if { [info exists images(sb-slider-h-grip)] } {
        ::ttk::style element create ${pfx}Horizontal.Scrollbar.grip image \
            [list $images(sb-slider-h-grip${sfx})] \
            -sticky {}
        set hasgrip true
      }

      ::ttk::style element create ${pfx}Horizontal.Scrollbar.thumb image \
          [list $images(sb-slider-hn${sfx}) \
          disabled $images(sb-slider-hd${sfx}) \
          {pressed !disabled} $images(sb-slider-hp${sfx}) \
          {active !pressed !disabled} $images(sb-slider-ha${sfx}) \
          ] \
          -border $imgbord \
          -padding 0 \
          -sticky ew

      set hlayout {}
      if { $colors(scrollbar.has.arrows) } {
        lappend hlayout {*}[list \
            ${pfx}Horizontal.Scrollbar.leftarrow -side left -sticky {} \
            ${pfx}Horizontal.Scrollbar.rightarrow -side right -sticky {} \
            ]
      }
      lappend hlayout {*}[list \
          ${pfx}Horizontal.Scrollbar.trough -sticky nsew -children [list \
          ${pfx}Horizontal.Scrollbar.thumb \
            _GRIP_ \
        ] \
      ]
      set grip {}
      if { $hasgrip } {
        set grip "-children { [list ${pfx}Horizontal.Scrollbar.grip -sticky {}] }"
      }
      regsub {_GRIP_} $hlayout $grip hlayout
      ::ttk::style layout ${pfx}Horizontal.TScrollbar $hlayout

      # vertical img border
      set imgbord [list [lindex $imgbord 1] [lindex $imgbord 0]]

      if { $colors(scrollbar.has.arrows) } {
        ::ttk::style element create ${pfx}Vertical.Scrollbar.uparrow image \
            [list $images(arrow-up-n${sfx}) \
            disabled  $images(arrow-up-d${sfx})]
        ::ttk::style element create ${pfx}Vertical.Scrollbar.downarrow image \
            [list $images(arrow-down-n${sfx}) \
            disabled  $images(arrow-down-d${sfx})]
      }
      if { $hasgrip && [info exists images(sb-slider-v-grip)] } {
        ::ttk::style element create ${pfx}Vertical.Scrollbar.grip image \
            [list $images(sb-slider-v-grip${sfx})] -sticky {}
      }

      ::ttk::style element create ${pfx}Vertical.Scrollbar.thumb image \
          [list $images(sb-slider-vn${sfx}) \
          disabled $images(sb-slider-vd${sfx}) \
          {pressed !disabled} $images(sb-slider-vp${sfx}) \
          {active !pressed !disabled} $images(sb-slider-va${sfx}) \
          ] \
          -border $imgbord \
          -padding 0 \
          -sticky ns

      if { [info exists images(sb-trough-hn)] } {
        set imgbord [_adjustSizes trough.image.border $scale]
        set imgpad [_adjustSizes trough.image.border $scale]
        ::ttk::style element create ${pfx}Horizontal.Scrollbar.trough image \
            [list $images(sb-trough-hn${sfx}) \
                disabled $images(sb-trough-hd${sfx})] \
            -border $imgbord \
            -padding 0 \
            -sticky ew

        set imgbord [list [lindex $imgbord 1] [lindex $imgbord 0]]
        ::ttk::style element create ${pfx}Vertical.Scrollbar.trough image \
            [list $images(sb-trough-vn${sfx}) \
                disabled $images(sb-trough-vd${sfx})] \
            -border $imgbord \
            -padding 0 \
            -sticky ns
      }

      set vlayout {}
      if { $colors(scrollbar.has.arrows) } {
        lappend vlayout {*}[list \
            ${pfx}Vertical.Scrollbar.uparrow -side top -sticky {} \
            ${pfx}Vertical.Scrollbar.downarrow -side bottom -sticky {} \
            ]
      }
      lappend vlayout {*}[list \
          ${pfx}Vertical.Scrollbar.trough -sticky nsew -children [list \
          ${pfx}Vertical.Scrollbar.thumb \
          _GRIP_ \
        ] \
      ]
      set grip {}
      if { $hasgrip } {
        set grip "-children { [list ${pfx}Vertical.Scrollbar.grip -sticky {}] }"
      }
      regsub {_GRIP_} $vlayout $grip vlayout
      ::ttk::style layout ${pfx}Vertical.TScrollbar $vlayout
    }

    ::ttk::style configure ${pfx}TScrollbar \
        -borderwidth 0 \
        -arrowsize 14
  }

  proc _createSizegrip { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(sizegrip)] } {
      ::ttk::style element create ${pfx}Sizegrip.sizegrip image $images(sizegrip${sfx})

      if { $pfx ne {} } {
        set layout [::ttk::style layout TSizegrip]
        regsub {(Sizegrip.sizegrip)} $layout "${pfx}\\1" layout
        ::ttk::style layout ${pfx}TSizegrip $layout
      }
    }
  }

  proc _createSpinbox { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(spin-arrow-down-n)] } {
      ::ttk::style element create ${pfx}Spinbox.uparrow image \
          [list $images(spin-arrow-up-n${sfx}) \
          disabled  $images(spin-arrow-up-d${sfx})]
      ::ttk::style element create ${pfx}Spinbox.downarrow image \
          [list $images(spin-arrow-down-n${sfx}) \
          disabled  $images(spin-arrow-down-d${sfx})]

      if { $pfx ne {} } {
        set layout [::ttk::style layout TSpinbox]
        regsub {(Spinbox.uparrow)} $layout "${pfx}\\1" layout
        regsub {(Spinbox.downarrow)} $layout "${pfx}\\1" layout
        ::ttk::style layout ${pfx}TSpinbox $layout
      }
    }

    if { $vars(have.tksvg) && [info exists images(entry-n)] } {
      # adjust the borders and padding by the scale factor
      set imgbord [_adjustSizes spinbox.image.border $scale]
      set imgpad [_adjustSizes spinbox.image.border $scale]

      ::ttk::style element create ${pfx}Spinbox.field image \
          [list $images(entry-n${sfx}) \
          disabled $images(entry-d${sfx}) \
          {focus !disabled} $images(entry-f${sfx}) \
          {hover !disabled} $images(entry-a${sfx}) \
          ] \
          -sticky nsew \
          -border $imgbord \
          -padding $imgpad \
    }

    ::ttk::style configure ${pfx}TSpinbox \
        -borderwidth 1 \
        -relief none \
        -arrowsize 14
  }

  proc _createToolbutton { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    # to support the breeze theme.
    # winxpblue gets into a tight loop on display if this is active (due
    # to imgborder sizes).
    if { $vars(have.tksvg) &&
        [info exists images(button-n)] &&
        $colors(toolbutton.use.button.image) } {

      # adjust the borders and padding by the scale factor
      set imgbord [_adjustSizes button.image.border $scale]
      set imgpad [_adjustSizes toolbutton.image.padding $scale]

      ::ttk::style element create ${pfx}Toolbutton.border image \
          [list $images(empty${sfx}) \
          {pressed !disabled} $images(button-p${sfx}) \
          {selected !disabled} $images(button-p${sfx}) \
          {active !disabled} $images(button-a${sfx}) \
          disabled $images(empty${sfx})] \
          -sticky news \
          -border $imgbord \
          -padding $imgpad
    }

    ::ttk::style configure ${pfx}Toolbutton \
        -width {} \
        -padding 0 \
        -borderwidth 1 \
        -relief flat
    ::ttk::style configure ${pfx}Toolbutton.label \
        -padding 0 \
        -relief flat
  }

  # Treeview
  #   Item
  #   Cell
  proc _createTreeview { {pfx {}} {sfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $vars(have.tksvg) && [info exists images(tree-arrow-down-n)] } {
      # states: selected, focus, user1 (open), user2 (leaf), alternate.
      # user1 : open
      # user2 : leaf
      # alternate : every other row
      ::ttk::style element create ${pfx}Treeitem.indicator image \
          [list $images(tree-arrow-right-n${sfx}) \
          {!user2 !user1 selected} $images(tree-arrow-right-sn${sfx}) \
          {user1 selected} $images(tree-arrow-down-sn${sfx}) \
          user1 $images(tree-arrow-down-n${sfx}) \
          user2 $images(empty${sfx})] \
          -sticky w

      if { $pfx ne {} } {
        set layout [::ttk::style layout Item]
        regsub {(Treeitem.indicator)} $layout "${pfx}\\1" layout
        ::ttk::style layout ${pfx}Item $layout
      }
    }
  }

  proc _createTheme { theme } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$theme $var $var
    }

    # the styling is set here.
    # the images are created here.
    # the colors are set in setStyledColors (called by scaledStyle)

    bind ComboboxListbox <Map> \
        +[list ::ttk::awthemes::awCboxHandler %W]
    # as awthemes does not ever change the fonts, it is up to the
    # main program to handle font changes for listboxes.

    ::ttk::style theme create $theme -parent $colors(parent.theme) -settings {
      _createScaledStyle $theme
    }
  }

  proc _createNotebookStyle { theme {pfx {}} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { ! $colors(tab.use.topbar) } {
      return
    }

    foreach {k bg} [list \
        tabindhover $colors(tab.color.hover) \
        tabindnotactive $colors(tab.color.inactive) \
        tabindselected $colors(tab.color.selected) \
        tabinddisabled $colors(tab.color.disabled) \
        ] {
      if { ! [info exists images(${pfx}$k)] } {
        set images(${pfx}$k) [image create photo \
            -width $vars(nb.img.width) \
            -height $vars(nb.img.height)]
        set row [lrepeat $vars(nb.img.width) $bg]
        set pix [list]
        for {set i 0} {$i < $vars(nb.img.height)} {incr i} {
          lappend pix $row
        }
        $images(${pfx}$k) put $pix
      }
    }

    if { ! [info exists vars(cache.nb.tabind.${pfx})] } {
      ::ttk::style element create \
         ${pfx}${theme}.Notebook.indicator image \
         [list $images(${pfx}tabindnotactive) \
         {hover active !selected !disabled} $images(${pfx}tabindhover) \
         {selected !disabled} $images(${pfx}tabindselected) \
         {disabled} $images(${pfx}tabinddisabled)]
      set vars(cache.nb.tabind.${pfx}) true
    }

    ::ttk::style layout ${pfx}TNotebook.Tab [list \
      Notebook.tab -sticky nswe -children [list \
        ${pfx}${theme}.Notebook.indicator -side top -sticky we \
        Notebook.padding -side top -sticky nswe -children [list \
          Notebook.focus -side top -sticky nswe -children {
            Notebook.label -side top -sticky {} \
          } \
        ] \
      ] \
    ]
  }

  proc _setStyledColors { {pfx {}} {scale 1.0} } {
    variable currtheme
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    set theme $vars(theme.name)

    ::ttk::style theme settings $theme {
      # defaults
      ::ttk::style configure . \
          -background $colors(bg.bg) \
          -bordercolor $colors(border) \
          -borderwidth 1 \
          -darkcolor $colors(bg.darker) \
          -fieldbackground $colors(entrybg.bg) \
          -focuscolor $colors(focus.color) \
          -foreground $colors(fg.fg) \
          -insertcolor $colors(entryfg.fg) \
          -lightcolor $colors(bg.lighter) \
          -relief none \
          -selectbackground $colors(select.bg) \
          -selectborderwidth 0 \
          -selectforeground $colors(select.fg) \
          -troughcolor $colors(trough.color) \
          -font TkDefaultFont
      ::ttk::style map . \
          -background [list disabled $colors(bg.bg)] \
          -foreground [list disabled $colors(fg.disabled)] \
          -selectbackground [list !focus $colors(select.bg)] \
          -selectforeground [list !focus $colors(select.fg)] \
          -bordercolor [list disabled $colors(border.disabled)]

      # button

      set pad [_adjustSizes button.padding $scale]
      ::ttk::style configure ${pfx}TButton \
          -bordercolor $colors(border.button) \
          -background $colors(button) \
          -lightcolor $colors(bg.lighter) \
          -darkcolor $colors(bg.darkest) \
          -anchor $colors(button.anchor) \
          -padding $pad

      ::ttk::style map ${pfx}TButton \
          -background [list \
              {pressed !disabled} $colors(button.pressed) \
              {active !disabled} $colors(button.active) \
              ] \
          -lightcolor [list pressed $colors(bg.darker)] \
          -darkcolor [list pressed $colors(bg.lighter)]

      # checkbutton

      set pad [_adjustSizes checkbutton.padding $scale]
      ::ttk::style configure ${pfx}TCheckbutton \
          -background $colors(bg.bg) \
          -lightcolor $colors(bg.light) \
          -darkcolor $colors(bg.dark) \
          -padding $pad \
          -focusthickness $colors(checkbutton.focusthickness)
      ::ttk::style configure ${pfx}Flexmenu.TCheckbutton \
          -padding $pad
      ::ttk::style map ${pfx}TCheckbutton \
          -background [list {hover !disabled} $colors(bg.active)] \
          -indicatorcolor [list selected $colors(bg.lightest)] \
          -darkcolor [list disabled $colors(bg.bg)] \
          -lightcolor [list disabled $colors(bg.bg)] \

      # combobox

      set pad [_adjustSizes combobox.padding $scale]
      ::ttk::style configure ${pfx}TCombobox \
          -foreground $colors(entryfg.fg) \
          -bordercolor $colors(border) \
          -lightcolor $colors(entrybg.bg) \
          -arrowcolor $colors(arrow.color) \
          -padding $pad
      ::ttk::style map ${pfx}TCombobox \
          -foreground [list disabled $colors(entryfg.disabled)] \
          -lightcolor [list \
              {disabled} $colors(entrybg.disabled) \
              focus $colors(focus.color.combobox) \
              ] \
          -darkcolor [list active $colors(graphics.color) \
              focus $colors(focus.color.combobox)] \
          -arrowcolor [list disabled $colors(arrow.color.disabled)] \
          -fieldbackground [list disabled $colors(entrybg.disabled)]

      # entry

      set pad [_adjustSizes entry.padding $scale]
      ::ttk::style configure ${pfx}TEntry \
          -foreground $colors(entryfg.fg) \
          -background $colors(bg.bg) \
          -bordercolor $colors(border) \
          -lightcolor $colors(entrybg.bg) \
          -padding $pad \
          -selectbackground $colors(select.bg) \
          -selectforeground $colors(select.fg)
      ::ttk::style map ${pfx}TEntry \
          -foreground [list disabled $colors(entryfg.disabled)] \
          -lightcolor [list active $colors(bg.bg) \
              disabled $colors(entrybg.disabled) \
              focus $colors(focus.color.entry)] \
          -fieldbackground [list disabled $colors(entrybg.disabled)]
      if { $::tcl_platform(os) eq "Darwin" } {
        # mac os x has cross-platform incompatibilities
        ::ttk::style configure ${pfx}TEntry \
            -background $colors(bg.dark)
        ::ttk::style map ${pfx}TEntry \
            -lightcolor [list active $colors(graphics.color) \
                focus $colors(focus.color)] \
            -background [list disabled $colors(bg.disabled)]
      }

      # frame

      ::ttk::style configure ${pfx}TFrame \
          -borderwidth 1 \
          -bordercolor $colors(bg.bg) \
          -lightcolor $colors(bg.lighter) \
          -darkcolor $colors(bg.darker)

      # label

      # labelframe

      # labelframe bordercolor does not work with 'default' based themes.
      # this is the setup for 'clam' based themes.
      ::ttk::style configure ${pfx}TLabelframe \
          -bordercolor $colors(border.labelframe) \
          -lightcolor $colors(bg.bg) \
          -darkcolor $colors(bg.bg)
      ::ttk::style map ${pfx}TLabelframe \
          -bordercolor [list disabled $colors(border.disabled)]

      # menubutton

      set pad [_adjustSizes menubutton.padding $scale]
      ::ttk::style configure ${pfx}TMenubutton \
          -arrowcolor $colors(arrow.color) \
          -padding $pad \
          -relief $colors(menubutton.relief) \
          -width $colors(menubutton.width)
      ::ttk::style map ${pfx}TMenubutton \
          -background [list {active !disabled} $colors(bg.active)] \
          -foreground [list disabled $colors(fg.disabled)] \
          -arrowcolor [list disabled $colors(arrow.color.disabled)]

      # notebook

      set pad [_adjustSizes notebook.tab.padding $scale]
      ::ttk::style configure ${pfx}TNotebook \
          -bordercolor $colors(border.tab) \
          -lightcolor $colors(tab.box) \
          -darkcolor $colors(bg.darker) \
          -relief none
      ::ttk::style configure ${pfx}TNotebook.Tab \
          -lightcolor $colors(tab.inactive) \
          -darkcolor $colors(tab.inactive) \
          -bordercolor $colors(border.tab) \
          -background $colors(tab.inactive) \
          -padding $pad \
          -focusthickness $colors(notebook.tab.focusthickness) \
          -relief none
      # the use of -lightcolor here turns off any relief.
      ::ttk::style map ${pfx}TNotebook.Tab \
          -bordercolor [list disabled $colors(border.tab)] \
          -foreground [list disabled $colors(fg.disabled)] \
          -background [list \
              {selected !disabled} $colors(tab.selected) \
              {!selected active !disabled} $colors(tab.active) \
              disabled $colors(tab.disabled)] \
          -lightcolor [list \
              {selected !disabled} $colors(tab.selected) \
              {!selected active !disabled} $colors(tab.active) \
              disabled $colors(tab.disabled)]

      # panedwindow

      set col $colors(bg.dark)
      if { $colors(is.dark) } {
        set col $colors(bg.darker)
      }
      ::ttk::style configure ${pfx}TPanedwindow \
          -background $col
      ::ttk::style configure Sash \
          -lightcolor $colors(highlight.darkhighlight) \
          -darkcolor $colors(bg.darkest) \
          -sashthickness \
          [expr {round(10*$vars(scale.factor)*$colors(scale.factor)*$scale)}]

      # progressbar

      ::ttk::style configure ${pfx}TProgressbar \
          -troughcolor $colors(trough.color) \
          -background $colors(bg.bg) \
          -bordercolor $colors(border.slider) \
          -lightcolor $colors(bg.lighter) \
          -darkcolor $colors(border.dark)
      ::ttk::style map ${pfx}TProgressbar \
          -troughcolor [list disabled $colors(bg.dark)] \
          -darkcolor [list disabled $colors(bg.disabled)] \
          -lightcolor [list disabled $colors(bg.disabled)]

      # radiobutton

      set pad [_adjustSizes radiobutton.padding $scale]
      ::ttk::style configure ${pfx}TRadiobutton \
          -padding $pad \
          -focusthickness $colors(radiobutton.focusthickness)
      ::ttk::style configure ${pfx}Flexmenu.TRadiobutton \
          -padding $pad
      ::ttk::style map ${pfx}TRadiobutton \
          -background [list {hover !disabled} $colors(bg.active)]

      # scale

      # background is used both for the background and
      # for the grip colors

      ::ttk::style configure ${pfx}TScale \
          -troughcolor $colors(trough.color) \
          -background $colors(bg.bg) \
          -bordercolor $colors(border.slider) \
          -lightcolor $colors(bg.lighter) \
          -darkcolor $colors(border.dark)
      ::ttk::style map ${pfx}TScale \
          -troughcolor [list disabled $colors(bg.dark)] \
          -darkcolor [list disabled $colors(bg.disabled)] \
          -lightcolor [list disabled $colors(bg.disabled)]

      # scrollbar

      ::ttk::style configure ${pfx}TScrollbar \
          -background $colors(bg.bg) \
          -bordercolor $colors(border.slider) \
          -lightcolor $colors(bg.lighter) \
          -darkcolor $colors(border.dark) \
          -arrowcolor $colors(arrow.color) \
          -troughcolor $colors(trough.color)
      ::ttk::style map ${pfx}TScrollbar \
          -troughcolor [list disabled $colors(bg.dark)] \
          -arrowcolor [list disabled $colors(arrow.color.disabled)] \
          -darkcolor [list disabled $colors(bg.bg)] \
          -lightcolor [list disabled $colors(bg.bg)]

      # separator

      ::ttk::style configure ${pfx}Horizontal.TSeparator \
          -background $colors(bg.bg) \
          -padding 0
      ::ttk::style configure ${pfx}Vertical.TSeparator \
          -background $colors(bg.bg) \
          -padding 0

      # spinbox

      set pad [_adjustSizes spinbox.padding $scale]
      ::ttk::style configure ${pfx}TSpinbox \
          -foreground $colors(entryfg.fg) \
          -bordercolor $colors(border) \
          -lightcolor $colors(entrybg.bg) \
          -darkcolor $colors(entrybg.bg) \
          -arrowcolor $colors(arrow.color) \
          -padding $pad
      ::ttk::style map ${pfx}TSpinbox \
          -foreground [list disabled $colors(entryfg.disabled)] \
          -lightcolor [list \
              disabled $colors(entrybg.disabled) \
              focus $colors(focus.color) \
              ] \
          -darkcolor [list \
              disabled $colors(entrybg.disabled) \
              focus $colors(focus.color) \
              ] \
          -arrowcolor [list disabled $colors(arrow.color.disabled)] \
          -fieldbackground [list disabled $colors(entrybg.disabled)]
      if { $::tcl_platform(os) eq "Darwin" } {
        # mac os x has cross-platform incompatibilities
        ::ttk::style configure ${pfx}TSpinbox \
            -background $colors(entrybg.bg)
        ::ttk::style map ${pfx}TSpinbox \
            -lightcolor [list active $colors(graphics.color) \
                {!focus !disabled} $colors(entrybg.bg) \
                {!focus disabled} $colors(entrybg.disabled) \
                focus $colors(focus.color)] \
            -darkcolor [list active $colors(graphics.color) \
                focus $colors(focus.color)] \
            -arrowcolor [list disabled $colors(arrow.color.disabled)] \
            -background [list disabled $colors(entrybg.disabled)]
      }

      # toolbutton

      if { ! $colors(toolbutton.use.button.image) } {
        ::ttk::style map ${pfx}Toolbutton \
            -background [list \
                {selected !disabled} $colors(toolbutton.bg) \
                {hover !selected !disabled} $colors(bg.active) \
                {disabled} $colors(bg.bg) \
                ]
      }

      # treeview

      ::ttk::style configure ${pfx}Treeview \
          -fieldbackground $colors(bg.bg) \
          -lightcolor $colors(bg.bg) \
          -bordercolor $colors(bg.bg)
      ::ttk::style map ${pfx}Treeview \
          -background [list selected $colors(select.bg.tree)] \
          -foreground [list selected $colors(select.tree.fg)]

      # do not want the focus ring.
      # removing it entirely fixes it on both linux and windows.
      set l [::ttk::style layout Item]
      if { [regsub "Treeitem.focus.*?-children \{" $l {} l] } {
        regsub "\}$" $l {} l
      }
      ::ttk::style layout Item $l

      ::ttk::style configure ${pfx}Heading \
          -background $colors(button) \
          -lightcolor $colors(bg.light) \
          -darkcolor $colors(bg.darkest) \
          -bordercolor $colors(border.button)
      ::ttk::style configure Heading \
          -font TkHeadingFont -relief raised
      ::ttk::style map ${pfx}Heading \
          -background [list \
              {active !disabled} $colors(bg.light) \
              ] \
          -lightcolor [list \
              {active !disabled} $colors(bg.lighter) \
              ]
    }
  }

  proc _setColors { {theme {}} } {
    variable currtheme
    if { $theme ne {} } {
      set currtheme $theme
    }
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    _setStyledColors
    dict for {w v} $vars(cache.menu) {
      if { [winfo exists $w] } {
        setMenuColors $w
      }
    }
    dict for {w t} $vars(cache.text) {
      if { [winfo exists $w] } {
        setTextColors $w $t
      }
    }
    dict for {w v} $vars(cache.listbox) {
      if { [winfo exists $w] } {
        setListboxColors $w
      }
    }

    _createNotebookStyle $currtheme {}
  }

  proc _adjustThemeColor { currtheme basename group oldcol newcol } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $group ni $::themeutils::vars(derived.group.names) } {
      return
    }

    foreach {grp k value type} $::themeutils::vars(names.colors.derived) {
      if { $grp ne $group } {
        continue
      } elseif { $colors($k) eq $colors($basename) } {
        set tc $newcol
      } elseif { [info exists colors(user.$k)] } {
        # do not override user-set colors
        set tc $colors(user.$k)
      } elseif { $k eq $basename || $k eq "${basename}.latest" || $colors($k) eq {} } {
        set tc $colors($k)
      } else {
        set tc [::colorutils::adjustColor $colors($k) $oldcol $newcol 2]
        if { $tc eq {} } {
          set tc $colors($k)
        }
      }
      set colors($k) $tc
    }
  }

  # sf : scale factor
  # pfx : prefix
  # sfx : suffix ( -$pfx )
  proc _createScaledStyle { {theme {}} {sf 1.0} {pfx {}} {sfx {}} } {
    variable currtheme

    set currtheme [::ttk::style theme use]
    if { $theme ne {} } {
      set currtheme $theme
    }
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }
    namespace upvar ::ttk::theme::$currtheme imgtype imgtype

    set starttime [clock milliseconds]
    foreach {n} [dict keys $vars(images)] {
      set scale $sf
      if { [regexp {small$} $n] || [regexp {pad$} $n] } {
        set scale [expr {$sf * 0.7}]
      }
      if { [regexp {^cb-} $n] } {
        set scale [expr {$sf * $colors(checkbutton.scale)}]
      }
      if { [regexp {^rb-} $n] } {
        set scale [expr {$sf * $colors(radiobutton.scale)}]
      }
      _mkimage $n${sfx} $scale
    }
    set endtime [clock milliseconds]
    if { $::ttk::awthemes::debug > 0 } {
      puts "t:     make images: [expr {$endtime-$starttime}]"
    }

    set starttime [clock milliseconds]
    ::ttk::style theme settings $vars(theme.name) {
      # arrows are used for scrollbars and spinboxes and possibly
      # the combobox.
      # the non-tksvg awdark and awlight themes define arrow-up-n
      if { [info exists images(arrow-up-n)] } {
        foreach {dir} {up down left right} {
          if { [info exists images(arrow-${dir}-n${sfx})] } {
            ::ttk::style element create ${pfx}${dir}arrow image \
                [list $images(arrow-${dir}-n${sfx}) \
                disabled $images(arrow-${dir}-d${sfx}) \
                pressed $images(arrow-${dir}-n${sfx}) \
                active $images(arrow-${dir}-n${sfx})] \
                -border 4 -sticky news
          }
        }
      }

      _createButton $pfx $sfx $sf
      _createCheckButton $pfx $sfx $sf
      _createCombobox $pfx $sfx $sf
      _createEntry $pfx $sfx $sf
      _createLabelframe $pfx $sfx $sf
      _createMenubutton $pfx $sfx $sf
      _createNotebook $pfx $sfx $sf
      _createProgressbar $pfx $sfx $sf
      _createRadioButton $pfx $sfx $sf
      _createScale $pfx $sfx $sf
      _createScrollbars $pfx $sfx $sf
      _createSizegrip $pfx $sfx $sf
      _createSpinbox $pfx $sfx $sf
      _createTreeview $pfx $sfx $sf
      _createToolbutton $pfx $sfx $sf
    }
    set endtime [clock milliseconds]
    if { $::ttk::awthemes::debug > 0 } {
      puts "t:     theme setup: [expr {$endtime-$starttime}]"
    }
    set starttime [clock milliseconds]
    _setStyledColors $pfx $sf
    set endtime [clock milliseconds]
    if { $::ttk::awthemes::debug > 0 } {
      puts "t:     set styled colors: [expr {$endtime-$starttime}]"
    }
  }

  # user routines

  proc hasImage { nm } {
    variable currtheme
    set currtheme [::ttk::style theme use]
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    set rc false
    if { $vars(have.tksvg) && [info exists images($nm)] } {
      set rc true
    }
    return $rc
  }

  proc getColor { theme nm } {
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$theme $var $var
    }

    set nm [::themeutils::_compatibilityConvert $nm]

    set rc {}
    if { [info exists colors($nm)] } {
      set rc $colors($nm)
    }
    return $rc
  }

  proc getScaleFactor { {pfx {}} } {
    variable currtheme
    set currtheme [::ttk::style theme use]
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $pfx ne {} } {
      append pfx .
    }
    set sf [expr {$vars(scale.factor)*$colors(${pfx}scale.factor)}]
    return $sf
  }

  proc setBackground { bcol {currtheme {}} } {
    set hastheme false
    if { $currtheme eq {} } {
      set currtheme [::ttk::style theme use]
      set hastheme true
    }
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { ! [package present colorutils] } {
      return
    }
    if { $colors(bg.bg.latest) eq $bcol } {
      return
    }

    _adjustThemeColor $currtheme bg.bg bg $colors(bg.bg.latest) $bcol

    set colors(bg.bg) $bcol
    set colors(bg.bg.latest) $bcol

    if { $hastheme } {
      _setColors $currtheme
    }
  }

  proc setHighlight { hcol {currtheme {}} } {
    set hastheme false
    if { $currtheme eq {} } {
      set currtheme [::ttk::style theme use]
      set hastheme true
    }
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { ! [package present colorutils] } {
      return
    }

    _adjustThemeColor $currtheme graphics.color accent \
        $colors(graphics.color.latest) $hcol

    set colors(graphics.color) $hcol
    set colors(graphics.color.latest) $hcol

    if { $hastheme } {
      _setColors $currtheme
    }
  }

  proc setMenuColors { w {theme {}} } {
    variable currtheme

    set currtheme [::ttk::style theme use]
    if { $theme ne {} } {
      set currtheme $theme
    }
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    # the standard menu does not have a -mode option
    if { $w ne "-optiondb" && [catch {$w cget -mode}] } {
      set max [$w index end]
      if { $max eq "none" } {
        return
      }

      # this does not work for mac os x standard menus.
      # it is fine for user menus or pop-ups
      for {set i 0} {$i <= $max} {incr i} {
        # margin must be hidden to hide the normal radio/checkbutton images
        set type [$w type $i]
        if { $type eq "checkbutton" } {
          $w entryconfigure $i \
              -image $images(cb-un-pad) \
              -selectimage $images(cb-sn-pad) \
              -compound left \
              -hidemargin 1
        }
        if { $type eq "radiobutton" } {
          $w entryconfigure $i \
              -image $images(rb-un-pad) \
              -selectimage $images(rb-sn-pad) \
              -compound left \
              -hidemargin 1
        }
      } ; # for each menu entry
    } ; # not using flexmenu? (has -mode)

    if { $w eq "-optiondb" } {
      option add *Menu.background $colors(menu.bg)
      option add *Menu.foreground $colors(menu.fg)
      option add *Menu.activeBackground $colors(select.bg)
      option add *Menu.activeForeground $colors(select.fg)
      option add *Menu.disabledForeground $colors(fg.disabled)
      option add *Menu.selectColor $colors(menu.fg)
      option add *Menu.relief $colors(menu.relief)
      # don't scale the borderwidth here
      option add *Menu.borderWidth 1
      return
    }

    if { ! [dict exists $vars(cache.menu) $w] } {
      dict set vars(cache.menu) $w 1
    }

    $w configure -background $colors(menu.bg)
    $w configure -foreground $colors(menu.fg)
    $w configure -activebackground $colors(select.bg)
    $w configure -activeforeground $colors(select.fg)
    $w configure -disabledforeground $colors(fg.disabled)
    $w configure -selectcolor $colors(menu.fg)
    $w configure -relief $colors(menu.relief)
    # don't scale the borderwidth here
    $w configure -borderwidth 1
  }

  proc setListboxColors { w {isforcombobox 0} {theme {}} } {
    variable currtheme

    set currtheme [::ttk::style theme use]
    if { $theme ne {} } {
      set currtheme $theme
    }
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { $w eq "-optiondb" } {
      option add *Listbox.background $colors(entrybg.bg)
      option add *Listbox.foreground $colors(entryfg.fg)
      option add *Listbox.disabledForeground $colors(fg.disabled)
      option add *Listbox.selectBorderWidth 0
      option add *Listbox.selectBackground $colors(select.bg)
      option add *Listbox.selectForeground $colors(select.fg)
      option add *Listbox.highlightColor $colors(focus.color)
      option add *Listbox.highlightBackground $colors(bg.bg)
      option add *Listbox.activeStyle none
      return
    }

    # the listbox cache is for re-setting the colors on
    # a color change, not for caching existing values.
    if { ! [dict exists $vars(cache.listbox) $w] } {
      dict set vars(cache.listbox) $w 1
    }

    $w configure -background $colors(entrybg.bg)
    $w configure -foreground $colors(entryfg.fg)
    $w configure -disabledforeground $colors(fg.disabled)
    $w configure -selectborderwidth 0
    $w configure -selectbackground $colors(select.bg)
    $w configure -selectforeground $colors(select.fg)
    $w configure -highlightcolor $colors(focus.color)
    $w configure -highlightbackground $colors(bg.bg)
    $w configure -activestyle none
    if { $isforcombobox } {
      # Want a border for the combobox drop-down.
      # The listbox border does not seem to have a method to change
      # its color.
      # -relief solid doesn't look too bad, but seems to only have
      # black as a color, and this doesn't always match the theme.
      # don't scale the borderwidth here
      $w configure -borderwidth 1
      if { $colors(is.dark) } {
        $w configure -relief solid
      }
    }
  }

  proc setTextColors { w {useflag {}} {theme {}} } {
    variable currtheme

    set currtheme [::ttk::style theme use]
    if { $theme ne {} } {
      set currtheme $theme
    }
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { ! [dict exists $vars(cache.text) $w] } {
      dict set vars(cache.text) $w $useflag
    }

    set tbg $colors(bg.bg)
    set tfg $colors(fg.fg)
    if { $useflag eq "-entry" } {
      set tbg $colors(entrybg.bg)
      set tfg $colors(entryfg.fg)
    } elseif { $useflag eq "-background" } {
      set tbg $colors(bg.bg)
      set tfg $colors(fg.fg)
    } elseif { $w ne "-optiondb" } {
      # if the user specifies -optiondb for the window, there is
      # no way to tell what color they want for the text widget
      if { [$w cget -state] eq "normal" } {
        set tbg $colors(entrybg.bg)
        set tfg $colors(entryfg.fg)
      } else {
        set tbg $colors(bg.bg)
        set tfg $colors(fg.fg)
      }
    }

    if { $w eq "-optiondb" } {
      option add *Text.background $tbg
      option add *Text.foreground $tfg
      option add *Text.selectForeground $colors(select.fg)
      option add *Text.selectBackground $colors(select.bg)
      option add *Text.insertBackground $tfg
      option add *Text.inactiveSelectBackground $colors(select.bg.inactive)
      option add *Text.highlightColor $colors(focus.color)
      option add *Text.highlightBackground $colors(bg.bg)
      return
    }

    $w configure -background $tbg
    $w configure -foreground $tfg
    $w configure -selectforeground $colors(select.fg)
    $w configure -selectbackground $colors(select.bg)
    $w configure -insertbackground $tfg
    $w configure -inactiveselectbackground $colors(select.bg.inactive)
    $w configure -highlightcolor $colors(focus.color)
    $w configure -highlightbackground $colors(bg.bg)
  }

  proc scaledStyle { {pfx {}} {f1 {}} {f2 {}} {theme {}} } {
    variable currtheme

    set starttime [clock milliseconds]
    set currtheme [::ttk::style theme use]
    if { $theme ne {} } {
      set currtheme $theme
    }
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }
    namespace upvar ::ttk::theme::$currtheme imgtype imgtype

    _initializeColors $currtheme

    set sf 1.0
    if { $f1 ne {} && $f2 ne {} } {
      set sz1 [font metrics $f1 -ascent]
      set sz2 [font metrics $f2 -ascent]
      set sf [expr {double($sz2)/double($sz1)}]
    }
    set sfx {}
    if { $pfx ne {} } {
      set colors(${pfx}.scale.factor) $sf
      set sfx -$pfx
      set pfx $pfx.
    }

    _setImageData $currtheme $sfx
    _createScaledStyle $currtheme $sf $pfx $sfx
    set endtime [clock milliseconds]
    if { $::ttk::awthemes::debug > 0 } {
      puts "t: scaled style $pfx: [expr {$endtime-$starttime}]"
    }
  }

  proc awCboxHandler { w } {
    variable currtheme
    set currtheme [::ttk::style theme use]
    foreach {var} {colors images imgdata vars} {
      namespace upvar ::ttk::theme::$currtheme $var $var
    }

    if { [info exists colors(entrybg.bg)] &&
        $currtheme eq $vars(theme.name) } {
      # get the combobox window name
      set cbw [winfo parent $w]
      set cbw [winfo parent $cbw]
      set cbw [winfo parent $cbw]
      set style [$cbw cget -style]
      if { $style eq {} } {
        set style TCombobox
      }
      if { [dict exists $vars(registered.combobox) $style] } {
        set pfx [dict get $vars(registered.combobox) $style]
        set sb [winfo parent $w].sb
        set sborient [string totitle [$sb cget -orient]]
        set sbstyle [$sb cget -style]
        if { $sbstyle eq {} } {
          set sbstyle ${sborient}.TScrollbar
        }
        $sb configure -style ${pfx}${sbstyle}
      }
      ::ttk::awthemes::setListboxColors $w true
    }
  }
}

namespace eval ::themeutils {
  variable vars

  proc _compatibilityConvert { cn } {
    # backwards compatibility settings
    # these do not cover all the name changes in 8.1/9.4; there are simply
    # too many due to the rework.  These are just a few of the major name
    # changes that have been mentioned in prior documentation.
    foreach {on nn chgversion} [list \
        base.bg bg.bg 8.1 \
        base.focus focus.color 8.1 \
        base.fg fg.fg 8.1 \
        selectbg.bg select.bg 9.4 \
        selectfg.fg select.fg 9.4 \
        highlight.active.bg select.bg 8.1,9.4 \
        ] {
      if { $cn eq $on } {
        set cn $nn
      }
    }
    return $cn
  }

  proc init {} {
    variable vars

    # style type, default style name.
    #   - a style name of "-" or "default" means to use the default
    #     The default will be (a) the fallback image, or (b) no graphics
    #     if no graphics are defined.
    #   - a style name of "none" means to use no graphics.
    #   - to make sure one of the awthemes defaults below is not used,
    #     set the style to "-" or "default".
    set vars(names.styles) {
        arrow           chevron
        button          -
        checkbutton     roundedrect-check
        combobox        -
        empty           empty
        entry           -
        labelframe      -
        menubutton      -
        notebook        -
        radiobutton     circle-circle
        scrollbar       -
        scrollbar-grip  circle
        scale           rect-bord-grip
        sizegrip        circle
        progressbar     rect-bord
        spinbox         -
        treeview        -
        trough          -
    }

    # These base colors must be set by the theme.
    # They are included in the bg/fg/accent groups below.
    set vars(names.colors.base) {
        bg.bg
        graphics.color
        fg.fg
        }

    # color-name, value, type
    # type is one of:
    #     base        a base colors, must already be set by the theme.
    #     static      a static value, set the color-name to value.
    #     color       a color name, set the color-name to colors($value)
    #     black       make a blend with black
    #     white       make a blend with white
    #     disabled    make a disabled blend with the background
    #     highlight   blend with black; blend with white if a dark theme
    #
    # for black, white, disabled and highlight the value is set to a list
    # with the color to blend with, and the percentage value.
    #
    # scale.trough may be a different color than the normal trough.
    # trough.color generally doesn't change, leave it in its own group.
    #

    set vars(derived.group.names) [list \
        bg fg entrybg entryfg focus accent selectfg other \
        ]

    set vars(names.colors.derived.basic) {
      bg      bg.dark                     {bg.bg 0.9}             black
      bg      bg.darker                   {bg.bg 0.7}             black
      bg      bg.darkest                  {bg.bg 0.5}             black
      bg      bg.light                    {bg.bg 0.95}            white
      bg      bg.lighter                  {bg.bg 0.8}             white
      bg      bg.lightest                 #ffffff                 static
    }

    # group
    # color name
    # data
    # type
    set vars(names.colors.derived) { \
      bg        bg.active                     {bg.bg 0.9}             highlight
      bg        bg.bg                         -                       base
      bg        bg.disabled                   {bg.bg 0.6}             disabled
      bg        border                        bg.darker               color
      bg        border.button.active          border.button           color
      bg        border.button                 border                  color
      bg        border.checkbutton            border                  color
      bg        border.dark                   bg.darkest              color
      bg        border.disabled               {border 0.8}            disabled
      bg        border.labelframe             border                  color
      bg        border.scale                  {scale.color 0.8}       black
      bg        border.slider                 border                  color
      bg        border.tab                    border                  color
      bg        button                        bg.bg                   color
      bg        button.active                 button                  color
      bg        button.active.focus           button.active           color
      bg        button.pressed                button                  color
      bg        menu.bg                       {bg.bg 0.9}             black
      bg        tab.active                    bg.light                color
      bg        tab.box                       bg.bg                   color
      bg        tab.disabled                  bg.darker               color
      bg        tab.inactive                  bg.dark                 color
      bg        tab.selected                  bg.bg                   color
      bg        toolbutton.bg                 {bg.bg 0.8}             black
      fg        fg.disabled                   {fg.fg 0.65}            disabled
      fg        fg.fg                         -                       base
      fg        menu.fg                       fg.fg                   color
      entrybg   entrybg.bg                    bg.dark                 color
      entrybg   entrybg.checkbutton           entrybg.bg              color
      entrybg   entrybg.disabled              {entrybg.bg 0.6}        disabled
      entryfg   entryfg.disabled              {entryfg.fg 0.6}        disabled
      entryfg   entryfg.fg                    fg.fg                   color
      focus     active.color                  graphics.color          color
      focus     focus.color.combobox          focus.color             color
      focus     focus.color.entry             focus.color             color
      focus     focus.color                   graphics.color          color
      focus     highlight.darkhighlight       {select.bg 0.9}         black
      focus     select.bg                     {focus.color 0.7}       white
      focus     select.bg.inactive            bg.lighter              color
      focus     select.bg.tree                select.bg               color
      accent    accent.color                  graphics.color          color
      accent    arrow.color.disabled          {arrow.color 0.6}       disabled
      accent    arrow.color                   graphics.color          color
      accent    combobox.color.arrow          arrow.color             color
      accent    graphics.color                -                       base
      accent    graphics.color.dark           {graphics.color 0.4}    black
      accent    graphics.color.light          {graphics.color 0.4}    white
      accent    graphics.highlight            accent.color            color
      accent    pbar.color                    graphics.color          color
      accent    pbar.color.border             graphics.color.dark     color
      accent    scale.color                   graphics.color          color
      accent    scrollbar.border              {scrollbar.color 0.8}   black
      accent    scrollbar.color               graphics.color          color
      accent    scrollbar.color.active        scrollbar.color         color
      accent    scrollbar.color.arrow         arrow.color             color
      accent    scrollbar.color.grip          graphics.color.dark     color
      accent    scrollbar.color.pressed       scrollbar.color         color
      accent    sizegrip.color                graphics.color          color
      accent    spinbox.color.arrow           arrow.color             color
      accent    spinbox.color.bg              graphics.color          color
      accent    spinbox.color.border          graphics.color.dark     color
      accent    tab.color.disabled            bg.lighter              color
      accent    tab.color.hover               graphics.color          color
      accent    tab.color.inactive            bg.darkest              color
      accent    tab.color.selected            graphics.color          color
      accent    tree.arrow.selected           arrow.color             color
      accent    trough.color                  {graphics.color 0.5}    black
      selectfg  select.fg                     bg.lightest             color
      selectfg  select.tree.fg                select.fg               color
      other     button.anchor                 w                       static
      other     button.has.focus              true                    static
      other     button.image.border           {1 1}                   static
      other     button.image.padding          {0 0}                   static
      other     button.padding                {4 1}                   static
      other     button.relief                 raised                  static
      other     checkbutton.focusthickness    2                       static
      other     checkbutton.padding           {5 0 1 2}               static
      other     checkbutton.scale             1.0                     static
      other     combobox.image.sticky         {}                      static
      other     combobox.image.border         {0 0}                   static
      other     combobox.entry.image.border   entry.image.border      color
      other     combobox.entry.image.padding  entry.image.padding     color
      other     combobox.padding              {3 1}                   static
      other     combobox.relief               none                    static
      other     debug                         0                       static
      other     entry.image.border            {1 1}                   static
      other     entry.image.padding           {0 0}                   static
      other     entry.padding                 {3 1}                   static
      other     menu.relief                   raised                  static
      other     menubutton.image.padding      {0 0}                   static
      other     menubutton.padding            {3 0}                   static
      other     menubutton.relief             none                    static
      other     menubutton.use.button.image   false                   static
      other     menubutton.width              {}                      static
      other     notebook.tab.borderwidth      0                       static
      other     notebook.tab.focusthickness   5                       static
      other     notebook.tab.padding          {1 0}                   static
      other     parent.theme                  clam                    static
      other     radiobutton.focusthickness    checkbutton.focusthickness color
      other     radiobutton.padding           checkbutton.padding     color
      other     radiobutton.scale             checkbutton.scale       color
      other     scale.factor                  1.0                     static
      other     scale.slider.image.border     slider.image.border     color
      other     scale.trough.image.border     trough.image.border     color
      other     scale.trough                  trough.color            color
      other     scrollbar.has.arrows          true                    static
      other     scrollbar.trough              trough.color            color
      other     slider.image.border           {0 0}                   static
      other     slider.image.padding          {0 0}                   static
      other     spinbox.image.border          entry.image.border      color
      other     spinbox.image.padding         entry.image.padding     color
      other     spinbox.padding               entry.padding           color
      other     tab.image.border              {2 2 2 1}               static
      other     tab.image.padding             {0 0}                   static
      other     tab.use.topbar                false                   static
      other     toolbutton.image.padding      {0 0}                   static
      other     toolbutton.use.button.image   false                   static
      other     trough.image.border           {0 0}                   static
      other     trough.image.padding          {0 0}                   static
    }

    # add the basic derived colors to the main list
    foreach {grp n value type} $vars(names.colors.derived.basic) {
      lappend vars(names.colors.derived) $grp $n $value $type
    }
  }

  proc awthemesVersion { } {
    return $::themeutils::awversion
  }

  proc setBackgroundColor { theme newbg } {
    ::themeutils::setThemeColors $theme bg.bg $newbg
  }

  proc setHighlightColor { theme newcol } {
    ::themeutils::setThemeColors $theme \
        graphics.color $newcol \
        focus.color $newcol \
        select.bg $newcol
  }

  proc setThemeColors { theme args } {
    variable vars

    namespace eval ::ttk::theme::$theme {}

    foreach {cn col} $args {
      set cn [::themeutils::_compatibilityConvert $cn]
      set ::ttk::theme::${theme}::colors(user.$cn) $col
    }
  }

  init
}
