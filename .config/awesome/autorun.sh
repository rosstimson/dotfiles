#!/bin/sh

# Helper function to determine if proces is already running before
# starting a new one.
function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

# These don't have long running processes so don't need to be run via
# the 'run' helper function.
#
# Set a standard PC keyboard with Colemak layout, and caps-locks is another
# control key.
setxkbmap us -variant colemak -option ctrl:nocaps

# Set touchpad options for tap to click and natural scrolling.
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Tapping Enabled' 1
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Natural Scrolling Enabled' 1

# Start long running processes using the helper function to ensure we
# don't get duplicates.
run urxvtd -q -o -f
run ssh-agent
run gpg-agent --daemon --allow-emacs-pinentry
run emacs --daemon
run mpd

