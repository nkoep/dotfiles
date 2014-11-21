#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Disable caps lock key.
setxkbmap -option ctrl:nocaps

# Disable caps lock.
numlockx on &

# Turn off the screensaver.
xset s off &
xset -dpms &

# Set the wallpaper.
WP=bluegradient.jpg
feh --bg-scale ~/Dropbox/Bilder/wallpaper/$WP &

# Start pulseaudio.
start-pulseaudio-x11

# Start the bottom xmobar instance.
~/.cabal/bin/xmobar &

# Start the system tray.
trayer-restart

~/Dropbox/bla/.bin/settings &

# Start xmonad.
exec xmonad

