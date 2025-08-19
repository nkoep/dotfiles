typeset -U path
path=(~/.bin ~/.bin_private ~/.work/bin ~/.local/bin $path)

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
  exec startx
fi
