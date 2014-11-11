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
feh --bg-scale $HOME/Dropbox/Bilder/wallpaper/$WP &

# TODO: Start the screensaver.
# xscreensaver -no-splash &

# Start pulseaudio.
start-pulseaudio-x11

# Start the bottom xmobar instance.
xmobar &

# Start the system tray.
trayer --width 10 --height 30 --align right --transparent true --alpha 0 \
    --tint 0x262729 &

# Start tray applications.
nm-applet &
volumeicon &
settings &

# Configure the screen layout.
xrandr --auto
# xrandr --output DisplayPort-0 --off --output DVI-1 --mode 1920x1080 \
#     --pos 1920x0 --rotate normal --output DVI-0 --mode 1920x1080 \
#     --pos 0x0 --rotate normal --output HDMI-0 --off

# Start xmonad.
exec xmonad

