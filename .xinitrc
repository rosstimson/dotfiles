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

# Set wallpaper
feh --bg-scale ~/Pictures/wallpapers/default &

# URxvt daemon
urxvtd -q -o -f &

# Emacs server/daemon
emacs --daemon &

# Removing the ugly x cursor
xsetroot -cursor_name left_ptr &

# Set a standard PC keyboard, British layout and swap ctrl / caps-lock.
setxkbmap -model pc104 -layout gb -option -option ctrl:nocaps &

# Set screensaver
#xautolock -time 5 -locker slock &

# Window managers / environments
i3

