#!/bin/sh

# Install configs.
mkdir -p ~/.config
for d in $(tree --noreport -id); do
  if [[ ! $d = "." ]]; then
    stow -t ~/ $d
  fi
done
