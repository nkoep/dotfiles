#!/bin/sh

# Move .zshrc file that gets installed by oh-my-zsh.
mv ~/.zshrc ~/.zshrc.bak

# Install configs.
mkdir -p ~/.config
for d in $(tree --noreport -id); do
  if [ ! $d = "." ] && [ ! $d = "firefox" ]; then
    stow -t ~/ $d
  fi
done
