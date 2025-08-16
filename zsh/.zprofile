typeset -U path
path=(~/.bin ~/.bin_private ~/.local/bin $path)

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
  exec startx
fi
