# Set the oh-my-zsh installation path.
export ZSH=$HOME/.oh-my-zsh

# Define the PATH variable.
typeset -U path
path=($HOME/Dropbox/bla/.bin $HOME/.cabal/bin $path)
if command -v ruby >/dev/null && command -v gem >/dev/null; then
    path=($(ruby -e 'puts Gem.user_dir')/bin $path)
fi

# Environment variables
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS="-R "
export BROWSER="chromium"
export EDITOR="vim"
export PYTHONSTARTUP="$HOME/.pystartup.py"
export TEXMFHOME="$HOME/.texmf"
export MOSEKLM_LICENSE_FILE="$HOME/Dropbox/bla/mosek.lic"
export VIDEO_FORMAT="PAL"
export SSH_KEY_PATH="~/.ssh/rsa_id"

