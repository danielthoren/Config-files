#!/bin/bash

# ------------------------------------------------------------------------------
# A few key remapping on GNU/Linux, for easier Emacs.
#
# Note: You need a querty keyboard in the first place.
#
# Replace :
#     - capslock by ctrl
#     - altgr    by alt
#
# If the AltGr rebind does not work: use the following command then press
# AltGr to get the keycode mapped to it:
#
# - xev -event keybaord
#
# Then replace the 'ISO_Level3_Shift' with the given keycode below.
# ------------------------------------------------------------------------------

# Set us layout
# setxkbmap -layout us

# Set caps lock as control
# setxkbmap -option ctrl:nocaps

# Use 'xmodmap' instead of 'setxkbmap' when mapping caps lock to
# control. When using 'setxkbmap', the mapping is reset when changing
# input language. The bindings set with 'xmodmap' is bound to a specific
# layout. Thus, swapping to another language removes the bindings, but
# they are restored when swapping back to the language in which they
# were set.

# NOTE: Deprecated by 'Xmodmap' config folder. Kept while validating

# Set Caps_Lock as Control
xmodmap -e 'clear lock'
xmodmap -e 'keysym Caps_Lock = Control_L'
xmodmap -e 'add control = Control_L'

# Set AltGr as Left alt
xmodmap -e 'clear mod5'
xmodmap -e 'add mod1 = ISO_Level3_Shift'
