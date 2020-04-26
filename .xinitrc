#!/bin/sh

xrdb -load $HOME/.Xresources
xset -b

# Set keyboard layout
setxkbmap us -variant colemak -option ctrl:nocaps

# Set media key bindings
xbindkeys

# Set touchpad options
xinput set-prop 'Synaptics TM3383-032' 'libinput Tapping Enabled' 1
xinput set-prop 'Synaptics TM3383-032' 'libinput Natural Scrolling Enabled' 1

# Hide cursor when it is not being used
xbanish &

# Set screen locker (10 mins)
xautolock -time 10 -locker slock &

# Start my custom status bar (Lemonbar-xft)
rt-bar &

# Start ssh-agent
# Only do this on Linux (Void) because OpenBSD defaults to starting the agent anyway.
if [ "$(uname -o)" = "GNU/Linux" ]; then
    if ! pgrep -u "$USER" ssh-agent > /dev/null; then
        ssh-agent > "$HOME/.ssh/ssh-agent.env"
    fi
    if [ -z "$SSH_AUTH_SOCK" ]; then
        eval "$(cat "$HOME/.ssh/ssh-agent.env")"
    fi
fi

# Start GPG Agent
gpg-agent --homedir /home/rosstimson/.gnupg --daemon &

# Start rxvt-unicode daemon
urxvtd -q -o -f &

# Start Emacs server/daemon
emacs --daemon &

# Start Dunst for notifications
dunst &

# Set wallpaper
feh --bg-scale ~/pictures/wallpapers/wallpaper

# WM
cwm
