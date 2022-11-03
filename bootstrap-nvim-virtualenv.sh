#!/bin/sh

export PYENV_ROOT="$HOME/.pyenv"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

pyenv install 3.7.13
pyenv virtualenv 3.7.13 neovim
pyenv shell neovim
pip install pynvim black==22.10.0 isort==5.10.1
