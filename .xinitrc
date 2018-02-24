#!/bin/sh

export LANG=en_GB.UTF-8 LC_ALL=en_GB.UTF-8

# Read in .Xresources
xrdb -merge ~/.Xresources

# Disable beep
xset b off

# Enable Toucpad tap and palm detection
#synclient TapButton1=1
synclient PalmDetect=1

eval $(gpg-agent --daemon --allow-emacs-pinentry)

# Set wallpaper
feh --bg-scale ~/Pictures/wallpapers/default

# URxvt daemon
urxvtd -q -o -f &

# Removing the ugly x cursor
xsetroot -cursor_name left_ptr

# Set a standard PC keyboard, British layout and swap ctrl / caps-lock.
setxkbmap -model pc104 -layout gb -option ctrl:nocaps

# Reverse scrolling similar to Mac OSX 'natural scrolling'.
xmodmap -e "pointer = 1 2 3 5 4"

# Set screensaver
#xautolock -time 5 -locker slock &

# Window managers / environments
#exec dwm

