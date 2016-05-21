#!/bin/sh

export LANG=en_GB.UTF-8 LC_ALL=en_GB.UTF-8

# Read in .Xresources
xrdb -merge ~/.Xresources

eval $(gpg-agent --daemon)

# Set wallpaper
feh --bg-scale ~/pictures/wallpapers/default

# URxvt daemon
urxvtd -q -o -f &

# Removing the ugly x cursor
xsetroot -cursor_name left_ptr

# Set keyboard to Apple Mac KB and swap ctrl / caps-lock.
# setxkbmap -model apple -layout us -variant mac -option ctrl:swapcaps

# Set a standard PC keyboard, British layout and swap ctrl / caps-lock.
setxkbmap -model pc104 -layout gb -option ctrl:nocaps

# Reverse scrolling similar to Mac OSX 'natural scrolling'.
xmodmap -e "pointer = 1 2 3 5 4"

# Set screensaver
xautolock -time 5 -locker "xlock -mode random +showdate -lockdelay 600 -timeout 20" &

# Window managers / environments
exec dwm

