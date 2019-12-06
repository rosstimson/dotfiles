#!/bin/sh

export ENV=${HOME}/.profile

xrdb -load $HOME/.Xresources
xset -b

# Set keyboard layout
setxkbmap us -variant colemak -option ctrl:nocaps

# Set touchpad options
# Natural scrolling, disable edge scrolling, enable palm detection, enable tap to click
if [ "$(uname -o)" = "GNU/Linux" ] && [ -x "$(command -v synclient)" ]; then
    synclient VertEdgeScroll=0 VertScrollDelta=-111 HorizEdgeScroll=0 HorizScrollDelta=-111 PalmDetect=1 PalmMinWidth=8 PalmMinZ=100 TapButton1=1 TapButton2=2 TapButton3=3
fi

# Hide cursor when it is not being used
xbanish &

# Start ssh-agent
# Only do this on Linux (Void) because OpenBSD defaults to starting the agent anyway.
if [ "$(uname -o)" = "GNU/Linux" ]
   if ! pgrep -u "$USER" ssh-agent > /dev/null; then
       ssh-agent > "$XDG_RUNTIME_DIR/ssh-agent.env"
   fi
   if [[ ! "$SSH_AUTH_SOCK" ]]; then
       eval "$(<"$XDG_RUNTIME_DIR/ssh-agent.env")"
   fi
fi

# Start GPG Agent
gpg-agent --homedir /home/rosstimson/.gnupg --use-standard-socket --allow-emacs-pinentry --daemon &

# Start rxvt-unicode daemon
urxvtd -q -o -f &

# Start Emacs server/daemon
emacs --daemon &

# Set wallpaper
feh --bg-scale ~/pictures/wallpapers/wallpaper

# WM
cwm
