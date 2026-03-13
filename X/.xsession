#!/bin/sh

# Disable the caps lock key.
setxkbmap -option ctrl:nocaps

# Enable global keybindings.
xbindkeys

# Enable the num pad.
numlockx on

# Launch udiskie for automatic mounts.
udiskie --smart-tray &

# Launch bluetooth tray.
blueberry-tray

# Turn off the screensaver.
xset s off
xset -dpms

# Set wallpaper.
feh --bg-fill ~/.wallpaper.jpg

# Pixel-perfect trackpad scrolling.
export MOZ_USE_XINPUT2=1

# Export theme colors.
export X_THEME_BG="#282A2E"
export X_THEME_FG="#C5C8C6"
export X_THEME_PRIMARY="#F0C674"

if [ "$(uname -n)" = "helge" ]; then
  export X_DPI=108
else
  export X_DPI=180
fi
xrdb -DX_DPI=$X_DPI -merge ~/.Xresources
