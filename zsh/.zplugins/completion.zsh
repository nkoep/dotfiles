autoload -U compinit
compinit -i

zmodload -i zsh/complist

WORDCHARS=''

bindkey -M menuselect "^o" accept-and-infer-next-history

unsetopt menu_complete
unsetopt flowcontrol
setopt auto_menu
setopt complete_in_word
setopt always_to_end

zstyle ":completion:*:*:*:*:*" menu select
zstyle ":completion:*" matcher-list "m:{[:lower:][:upper:]}={[:upper:][:lower:]}" "r:|=*" "l:|=* r:|=*"
# Complete . and .. special directories.
zstyle ":completion:*" special-dirs true
zstyle ":completion:*" list-colors ""
zstyle ":completion:*:*:kill:*:processes" list-colors "=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01"
zstyle ":completion:*:*:*:*:processes" command "ps -u $USERNAME -o pid,user,comm -w -w"
# Disable named-directories autocompletion.
zstyle ":completion:*:cd:*" tag-order local-directories directory-stack path-directories
# Use caching so that commands like apt and dpkg complete are usable.
zstyle ":completion:*" use-cache yes
zstyle ":completion:*" cache-path $ZSH_CACHE_DIR
zstyle "*" single-ignored show

autoload -U +X bashcompinit && bashcompinit

[[ -z "$LS_COLORS" ]] || zstyle ":completion:*" list-colors "${(s.:.)LS_COLORS}"
