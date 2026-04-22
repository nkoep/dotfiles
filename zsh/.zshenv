export BROWSER="firefox"
export EDITOR="$VISUAL"
export GPG_TTY=$(tty)
export LESS="-R "
export MKL_THREADING_LAYER=GNU
export TERM=xterm-256color
export TEXMFHOME="$HOME/.texmf"
export VISUAL="nvim"

if [[ -x /usr/bin/lesspipe ]]; then
  eval "$(lesspipe)"
elif [[ -x /usr/bin/lesspipe.sh ]]; then
  eval "$(lesspipe.sh)"
fi

if [[ -e ~/.work/zshenv ]]; then
  source ~/.work/zshenv
fi
