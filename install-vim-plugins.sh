#!/bin/sh

if ! command -v nvim >/dev/null; then
  echo "Neovim is not installed"
  exit 1
fi

nvim +PlugInstall +qall
