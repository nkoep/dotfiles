#!/bin/sh

for p in tree curl nvim; do
  if ! command -v "$p" >/dev/null; then
    echo "Utility '$p' is not installed"
    exit 1
  fi
done

# Grab oh-my-zsh.
if [ ! -d ~/.oh-my-zsh ]; then
  sh -c "$(curl -fsSL \
    https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

# Install vim-plug for vim.
PLUG_FILE="~/.local/share/nvim/site/autoload/plug.vim"
if [ ! -e $PLUG_FILE ]; then
  curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

# Symlink files for neovim.
mkdir -p ~/.config
