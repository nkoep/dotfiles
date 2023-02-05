#!/bin/sh
#
for p in tree stow; do
  if ! command -v "$p" >/dev/null; then
    echo "$p is not installed"
    exit 1
  fi
done


# Move .zshrc file that gets installed by oh-my-zsh.
mv ~/.zshrc ~/.zshrc.bak

# Install configs.
mkdir -p ~/.config
for d in $(tree --noreport -id); do
  if [ ! $d = "." ] && [ ! $d = "firefox" ]; then
    stow -t ~/ $d
  fi
done
