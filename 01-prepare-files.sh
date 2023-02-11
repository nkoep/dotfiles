#!/bin/sh

if ! command -v curl >/dev/null; then
  echo "curl is not installed"
  exit 1
fi

# Grab oh-my-zsh.
if [ ! -d ~/.oh-my-zsh ]; then
  sh -c "$(curl -fsSL \
    https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
fi

# Symlink files for neovim.
mkdir -p ~/.config
