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
# Set a standard PC keyboard with US layout, and caps-locks is another
# control key.
setxkbmap -model pc104 -layout "us" -option ctrl:nocaps

# Start long running processes using the helper function to ensure we
# don't get duplicates.
run urxvtd -q -o -f
run ssh-agent
run gpg-agent --daemon --allow-emacs-pinentry
run emacs --daemon
run mpd
