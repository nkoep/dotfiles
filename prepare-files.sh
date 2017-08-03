#!/bin/sh

for p in tree curl vim; do
  if ! command -v "$p" >/dev/null; then
    echo "Utility '$p' is not installed"
    exit 1
  fi
done

# Grab oh-my-zsh.
if [ ! -d ~/.oh-my-zsh ]; then
  sh -c "$(curl -fsSL \
    https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
fi

# Install vim-plug for vim.
if [ ! -e ~/.vim/autoload/plug.vim ]; then
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# Symlink files for neovim.
ln -sf ~/.vim ~/.config/nvim
ln -sf ~/.vimrc ~/.config/nvim/init.vim
