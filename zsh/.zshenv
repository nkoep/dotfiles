# Set the oh-my-zsh installation path.
export ZSH=$HOME/.oh-my-zsh

# Define the PATH variable.
typeset -U path
path=(~/.bin ~/.bin_private ~/.local/bin $path)

# Environment variables
export BROWSER="firefox"
export EDITOR="$VISUAL"
export GPG_TTY=`tty`
export LESS="-R "
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export MKL_THREADING_LAYER=GNU
export TEXMFHOME="$HOME/.texmf"
export VISUAL="nvim"

# Source pyenv.
export PYENV_ROOT="$HOME/.pyenv"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

if [ -e ~/.work/zshenv ]; then
  source ~/.work/zshenv
fi
