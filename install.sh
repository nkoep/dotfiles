#!/bin/sh

for p in tree stow; do
  if ! command -v "$p" >/dev/null; then
    echo "$p is not installed"
    exit 1
  fi
done

# Move .zshrc file that gets installed by oh-my-zsh.
zshrc="$HOME/.zshrc"
if [ -f "$zshrc" ]; then
  mv "$zshrc" "${zshrc}.bak"
fi

# Install configs.
mkdir -p ~/.config
for directory in $(tree --noreport -id); do
  if [ ! "$directory" = "." ] && [ ! "$directory" = "firefox" ]; then
    stow -t ~/ "$directory"
  fi
done
