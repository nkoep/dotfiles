# Check for an interactive session
[ -z "$PS1" ] && return

# Color codes
RESET="\[\033[0m\]"
BLACK="\[\033[0;30m\]"
GRAY="\[\033[1;30m\]"

# Prompt
PS1="$BLACK[$GRAY\w$BLACK] » $RESET"

# terminfo
if [[ -e /usr/share/terminfo/x/xterm-256color ]]; then
    export TERM="xterm-256color"
fi

# Environment variables
export PATH=$HOME/Dropbox/bla/.bin:$HOME/.cabal/bin:$PATH
if command -v ruby >/dev/null && command -v gem >/dev/null; then
    PATH="$(ruby -e 'puts Gem.user_dir')/bin:$PATH"
fi
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS="-R "
export BROWSER=chromium
export EDITOR=vim
export PYTHONSTARTUP="$HOME/.pystartup"

# Aliases
alias less="less -R"
alias ls="ls -h --color=always --group-directories-first"
alias l="ls -lF"
alias la="ls -lAF"
alias grep="grep -E"
alias changes="svn log -r HEAD -v"
alias which="command -v"
alias pmsyu="sudo pacman -Syu"
alias pms="sudo pacman -S"
alias pmss="pacman -Ss"
alias pmsi="sudo pacman -Si"
alias pmqs="pacman -Qs"
alias pmqi="pacman -Qi"
alias pmqo="pacman -Qo"
alias pmql="pacman -Ql"
alias pmrs="sudo pacman -Rs"
alias pmpl="pacman -Qqe"
alias pcc="paccache -d -k2 -vvv"
alias valgrind="valgrind --leak-check=full"

