# Define the PATH variable.
typeset -U path
path=(~/.bin ~/.bin_private ~/.local/bin $path)

# Environment variables
export BROWSER="firefox"
export EDITOR="$VISUAL"
export GPG_TTY=$(tty)
export LESS="-R "
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export MKL_THREADING_LAYER=GNU
export TERM=xterm-256color
export TEXMFHOME="$HOME/.texmf"
export VISUAL="nvim"

if [[ -e ~/.work/zshenv ]]; then
  source ~/.work/zshenv
fi
