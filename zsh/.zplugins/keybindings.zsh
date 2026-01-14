bindkey -e

# Start typing + [Up-Arrow] - fuzzy find history forward
autoload -U up-line-or-beginning-search
zle -N up-line-or-beginning-search

bindkey "^[[A" up-line-or-beginning-search
if [[ -n ${terminfo[kcuu1]} ]]; then
  bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi

# Start typing + [Down-Arrow] - fuzzy find history backward
autoload -U down-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey "^[[B" down-line-or-beginning-search
if [[ -n ${terminfo[kcud1]} ]]; then
  bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

# [Shift-Tab] - move through the completion menu backwards
if [[ -n ${terminfo[kcbt]} ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete
fi

# [Delete] - delete forward
if [[ -n ${terminfo[kdch1]} ]]; then
  bindkey "${terminfo[kdch1]}" delete-char
fi

# [Ctrl-Delete] - delete whole forward-word
bindkey "^[[3;5~" kill-word

# [Ctrl-RightArrow] - move forward one word
bindkey "^[[1;5C" forward-word
# [Ctrl-LeftArrow] - move backward one word
bindkey "^[[1;5D" backward-word

bindkey "^r" history-incremental-search-backward
bindkey " " magic-space

if command -v fzf >/dev/null; then
  source <(fzf --zsh)
fi
