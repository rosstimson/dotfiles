#!/bin/sh

export LANG=en_GB.UTF-8 LC_ALL=en_GB.UTF-8

# Read in .Xresources
xrdb -merge ~/.Xresources &

# Disable beep
xset b off &

# Touchpad settings inc. natural scrolling
#synclient TapButton1=1 &
synclient PalmDetect=1 &
synclient VertScrollDelta=-111 &
synclient HorizScrollDelta=-111 &

gpg-agent --daemon --allow-emacs-pinentry &

# URxvt daemon
urxvtd -q -o -f &

# Emacs server/daemon
emacs --daemon &

# Removing the ugly x cursor
xsetroot -cursor_name left_ptr &

# Set a standard PC keyboard, US and British layouts which can be
# toggled with Shift-ALT, and swap ctrl / caps-lock.
setxkbmap -model pc104 -layout "us,gb" -option ctrl:nocaps -option "grp:alt_shift_toggle" &

# Set screensaver
#xautolock -time 5 -locker slock &

# Remove the mouse pointer when idle.
unclutter &

# dstat is a dwm status bar utility for OpenBSD.
dstat iwm0 &

# Set wallpaper
feh --bg-scale ~/Pictures/wallpapers/default &

# Window managers / environments
# i3
dwm
