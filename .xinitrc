#!/bin/sh

export ENV=${HOME}/.kshrc

xrdb -load $HOME/.Xresources
xset -b
setxkbmap us -variant colemak -option ctrl:nocaps

# Hide cursor when it is not being used
xbanish &

# Start GPG Agent
gpg-agent --homedir /home/rosstimson/.gnupg --use-standard-socket --allow-emacs-pinentry --daemon &

# Set wallpaper
feh --bg-scale ~/pictures/wallpapers/wallpaper

# WM
cwm
