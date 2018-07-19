#!/bin/sh

export LANG=en_GB.UTF-8 LC_ALL=en_GB.UTF-8

# Read in .Xresources
xrdb -merge ~/.Xresources &

# Disable beep
xset -b &

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

# Set a standard PC keyboard with US layout, and caps-locks is another
# control key.
setxkbmap us -option ctrl:nocaps &

# Set screensaver
#xautolock -time 5 -locker slock &

# Remove the mouse pointer when idle.
unclutter &

# dstat is a dwm status bar utility for OpenBSD.
dstat iwm0 &

# Adjust screen colours/temperature at night time.
redshift -l 55.61674:-2.81172 -g 0.9 &

# Set wallpaper
feh --bg-scale ~/Pictures/wallpapers/default &

# Window managers / environments
#i3
#dwm
awesome
