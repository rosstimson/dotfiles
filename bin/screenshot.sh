#!/bin/sh
#

screenshot() {
  case $1 in
  full)
    scrot -m -e 'mv $f ~/Pictures/Screenshots/'
    ;;
  window)
    sleep 1
    scrot -s -e 'mv $f ~/Pictures/Screenshots/'
    ;;
  *)
    ;;
  esac;
}

screenshot $1
