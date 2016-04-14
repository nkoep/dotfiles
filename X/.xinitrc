#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

# Disable the caps lock key.
setxkbmap -option ctrl:nocaps

# Enable the num pad.
numlockx on &

# Turn off the screensaver.
xset s off &
xset -dpms &

# Set the wallpaper.
WP=bluegradient.jpg
feh --bg-scale ~/Dropbox/Bilder/wallpaper/$WP &

# Start remaining background processes.
start-pulseaudio-x11
xrr-events --daemonize &
volumeicon &
dropbox-restart &

icc="~/.color/icc/benq_xl2411.icm"
if [ -e "$icc" ]; then
  dispwin -d0 "$icc"
fi

# Start xmonad.
exec xmonad
