setopt extendedglob

for plugin in "$HOME/.zplugins"/*.zsh(N); do
  source "$plugin"
done

if [[ -e ~/.zshenv_keys ]]; then
  source ~/.zshenv_keys
fi
if [[ -e ~/.work/zshrc ]]; then
  source ~/.work/zshrc
fi
