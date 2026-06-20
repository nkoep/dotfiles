typeset -U path
path=(
  ~/.bin
  ~/.work/bin
  ~/.local/bin
  $path
)

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
  exec startx
fi
