#!/bin/sh

numlockx on &
xset s off &
xset -dpms &
xrandr --auto --output DVI-0 --left-of DVI-1 &

# Set the wallpaper.
WP=nightshade31920x1080_xx.jpg
feh --bg-scale /home/nik/Dropbox/Bilder/wallpaper/$WP &

# Start the panel.
tint2 -c /home/nik/.themes/blasuite/tintrc &

# Start conky scripts.
conky -c /home/nik/.conky/conkyrc &
conky -c /home/nik/.conky/conkyrc_albumart &

volumeicon &
thunderbird &
/home/nik/Dropbox/bla/.bin/settings &
(sleep 5s && xchat) &

