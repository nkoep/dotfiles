#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

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

# Start pulseaudio.
start-pulseaudio-x11

# Start the bottom xmobar instance.
~/.cabal/bin/xmobar &

~/Dropbox/bla/.bin/settings &

xrr-events --daemonize &

# Start xmonad.
exec ~/.cabal/bin/xmonad

