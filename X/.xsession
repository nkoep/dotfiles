#!/bin/sh

# Disable the caps lock key.
setxkbmap -option ctrl:nocaps

# Set up global keybindings.
xbindkeys

# Enable the num pad.
numlockx on &

# Kill polybar just in case.
pkill polybar

# Turn off the screensaver.
xset s off &
xset -dpms &

# Start remaining background processes.
nm-applet &
dropbox &

case $HOSTNAME in
  monolith)
    export X_DPI=180
    export X_BAR_HEIGHT=40
    ;;
  helge)
    export X_DPI=110
    export X_BAR_HEIGHT=30
    ;;
  *)
    export X_DPI=96
    export X_BAR_HEIGHT=30
  ;;
esac

xrdb -DX_DPI=$X_DPI -merge ~/.Xresources
