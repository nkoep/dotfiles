#!/bin/sh

# Grab oh-my-zsh.
if [ ! -d ~/.oh-my-zsh ]; then
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
fi

# Install configs.
mkdir -p ~/.config
for d in $(tree --noreport -id); do
  if [[ ! $d = "." ]]; then
    stow -t ~/ $d
  fi
done

# Grab vim plug and fetch plugins.
if [ ! -e ~/.vim/autoload/plug.vim ]; then
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  vim +PlugInstall +qall
fi
