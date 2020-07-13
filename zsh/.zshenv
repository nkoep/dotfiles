# Set the oh-my-zsh installation path.
export ZSH=$HOME/.oh-my-zsh

# Define the PATH variable.
typeset -U path
path=(~/.bin ~/.bin_private ~/.local/bin ~/.cabal/bin $path)
if command -v ruby >/dev/null && command -v gem >/dev/null; then
  path=($(ruby -e 'puts Gem.user_dir')/bin $path)
fi

# Add conda command.
condapath="/home/nik/.anaconda3/etc/profile.d/conda.sh"
if [ -e "$condapath" ]; then
  source "$condapath"
fi

# Source pyenv and pyenv-virtualenv.

if command -v pyenv >/dev/null; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# Set up OPAM.
test -r /home/nik/.opam/opam-init/init.zsh && \
  . /home/nik/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Environment variables
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS="-R "
export BROWSER="firefox"
if [ "$(command -v nvim)" != "" ] ;then
  export VISUAL="nvim"
else
  export VISUAL="vim"
fi
export EDITOR="$VISUAL"
export PYTHONSTARTUP="$HOME/.pystartup.py"
export TEXMFHOME="$HOME/.texmf"
export VIDEO_FORMAT="PAL"
export MKL_THREADING_LAYER=GNU
