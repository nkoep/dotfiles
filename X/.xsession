#!/bin/sh

# Disable the caps lock key.
setxkbmap -option ctrl:nocaps

# Enable global keybindings.
xbindkeys

# Enable the num pad.
numlockx on

# Kill polybar just in case (gets started by xmonad).
pkill polybar

# Launch udiskie for automatic mounts.
udiskie --smart-tray &

# Launch bluetooth tray.
blueman-applet &

# Restart netctl.
sudo systemctl restart netctl-auto@wlp58s0 &

# Turn off the screensaver.
xset s off
xset -dpms

case $HOST in
  monolith)
    export X_DPI=180
    ;;
  *)
    export X_DPI=96
    ;;
esac

xrdb -DX_DPI=$X_DPI -merge ~/.Xresources
