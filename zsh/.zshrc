setopt extendedglob

for plugin in "$HOME/.zplugins"/*.zsh(N); do
  source "$plugin"
done

if [[ -e ~/.work/zshrc ]]; then
  source ~/.work/zshrc
fi
