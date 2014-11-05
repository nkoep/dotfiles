#!/bin/sh

numlockx on &
xset s off &
xset -dpms &

# Set the wallpaper.
WP=bluegradient.jpg
feh --bg-scale $HOME/Dropbox/Bilder/wallpaper/$WP &

# Start any remaining applications.
/usr/lib/cinnamon-settings-daemon/cinnamon-settings-daemon &
nm-applet &
tint2 &
volumeicon &
thunderbird &

