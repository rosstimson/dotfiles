#!/bin/sh
#

screenshot() {
  case $1 in
  full)
    scrot -m -e 'mv $f ~/pictures/screenshots/'
    ;;
  window)
    sleep 1
    scrot -s -e 'mv $f ~/pictures/screenshots/'
    ;;
  *)
    ;;
  esac;
}

screenshot $1
