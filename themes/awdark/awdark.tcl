#!/usr/bin/tclsh

set themeRoot "aw-themes"

set infoScript [info script]
set themeFilename [file tail $infoScript]
set themeFilepath [file join [file dirname $infoScript] .. $themeRoot $themeFilename]

source $themeFilepath
