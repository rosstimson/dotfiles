#!/bin/sh

# Need to add my scripts location to path specifically for Xorg.  The PATH is
# set elsewhere for my shell. Without this the top bar 'rt-bar' doesn't work at
# startup and neither do the Spofity keybinding in FVWM which rely on the 'sp'
# script in '~/bin'.
export PATH=$HOME/bin:$PATH

xrdb -load $HOME/.Xresources
xset -b

# Set US keyboard layout and toggle between standard QWERTY and
# Colemak layouts by pressing both CTRL keys together, this is helpful
# when I use my Ergodox keyboard which is configured to do Colemak but
# needs to have the OS set to standard US layout for it to work.
setxkbmap -layout 'us,us' \
	-variant 'colemak,' \
	-option 'grp:ctrls_toggle,ctrl:nocaps'

# Set media key bindings
xbindkeys

# Set touchpad options
if grep -q 'ID=ubuntu' /etc/os-release && command -v synclient; then
	synclient PalmDetect=1 \
			  PalmMinWidth=8 \
			  PalmMinZ=100 \
			  HorizScrollDelta=-111 \
			  VertScrollDelta=-111 \
			  HorizEdgeScroll=0 \
			  VertEdgeScroll=0 \
			  TapButton1=1 \
			  TapButton2=0 \
			  RTCornerButton=0 \
			  RBCornerButton=0 \
			  FingerHigh=50 \
			  PressureMotionMinZ=80
else
	xinput set-prop 'Synaptics TM3383-032' 'libinput Tapping Enabled' 1
	xinput set-prop 'Synaptics TM3383-032' 'libinput Natural Scrolling Enabled' 1
fi

# Hide cursor when it is not being used
xbanish &

# Set screen locker (10 mins)
xautolock -time 10 -locker slock &

# Start my custom status bar (Lemonbar-xft)
rt-bar &

# Start GPG Agent
gpg-agent --homedir /home/rosstimson/.gnupg --daemon &

# Start rxvt-unicode daemon
urxvtd -q -o -f &

# Start Emacs server/daemon
emacs --daemon &

# Start Dunst for notifications
dunst &

# Set wallpaper
feh --bg-scale ~/Pictures/wallpapers/wallpaper

# WM
fvwm
