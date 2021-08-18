#!/bin/sh

# Disable the caps lock key.
setxkbmap -option ctrl:nocaps

# Enable global keybindings.
xbindkeys

# Enable the num pad.
numlockx on

# Kill polybar just in case (gets started by xmonad).
pkill polybar

# Restart netctl.
sudo systemctl restart netctl-auto@wlp58s0 &

# Turn off the screensaver.
xset s off
xset -dpms

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
