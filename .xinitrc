#!/bin/sh

export LANG=en_GB.UTF-8 LC_ALL=en_GB.UTF-8

# Read in .Xresources
# [[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Set wallpaper
feh --bg-scale ~/Pictures/wallpapers/Take_Me_Elsewhere-1600x1200.jpg &

# URxvt daemon
urxvtd -q -o -f &

# Removing the ugly x cursor
xsetroot -cursor_name left_ptr &

# Set keyboard to Apple Mac KB and swap ctrl / caps-lock.
# setxkbmap -model apple -layout us -variant mac -option ctrl:swapcaps &

# Set a standard PC keyboard, British layout and swap ctrl / caps-lock.
setxkbmap -model pc104 -layout gb -option ctrl:swapcaps &

# Reverse scrolling similar to Mac OSX Lion 'natural scrolling'.
xmodmap -e "pointer = 1 2 3 5 4"

# Set screensaver
xscreensaver -no-splash &

# Start misc apps
numlockx &
# exec conky &

# Window managers / environments
# exec openbox-session
# exec spectrwm
# exec startxfce4
#
# Enable a choice within SLiM, defaults to OpenBox (experimental)
# Press F1 in SLiM to select
# DEFAULT_SESSION=spectrwm
#
# case $1 in
# openbox)
#         exec openbox-session
#         ;;
# xfce4)
#         exec startxfce4
#         ;;
# *)
#         exec $DEFAULT_SESSION
#         ;;
# esac
