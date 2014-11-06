# Set the oh-my-zsh installation path.
export ZSH=$HOME/.oh-my-zsh

# Set the theme.
ZSH_THEME="bla"
# ZSH_THEME="mh"
# ZSH_THEME="arrow"
# ZSH_THEME="cloud"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Select zsh plugins.
plugins=(colored-man cp git systemd z)

# Define the PATH variable.
typeset -U path
path=($HOME/Dropbox/bla/.bin $HOME/.cabal/bin $path)
if command -v ruby >/dev/null && command -v gem >/dev/null; then
    path=($(ruby -e 'puts Gem.user_dir')/bin $path)
fi

source $ZSH/oh-my-zsh.sh

# terminfo
if [[ -e /usr/share/terminfo/x/xterm-256color ]]; then
    export TERM="xterm-256color"
fi

# Environment variables
export LESSOPEN="| /usr/bin/source-highlight-esc.sh %s"
export LESS="-R "
export BROWSER="chromium"
export EDITOR="vim"
export PYTHONSTARTUP="$HOME/.pystartup"
export TEXMFHOME="$HOME/.texmf"
export MOSEKLM_LICENSE_FILE="$HOME/Dropbox/bla/mosek.lic"
export VIDEO_FORMAT="PAL"
export SSH_KEY_PATH="~/.ssh/rsa_id"

# Aliases
alias clang++11="clang++ -stdlib=libc++ -std=c++11 -lc++ -lc++abi"
alias less="less -R"
alias ls="ls -h --color=always --group-directories-first"
alias l="ls -lF"
alias la="ls -lAF"
alias grep="grep -E"
alias changes="svn log -r HEAD -v"
alias which="command -v"
alias pmsyu="sudo pacman -Syu"
alias pmu="sudo pacman -U"
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
alias python="python -q"
alias valgrind="valgrind --leak-check=full"
alias octmod="stat -c %a"
alias revparse="git rev-parse --short"
alias encoding="file -b --mime-encoding"
alias yearn="journalctl" # Do you ever yearn?

# Functions
function cdup() {
    if ! [[ $1 =~ [0-9]+ && $1 -gt 0 ]]; then
        echo "argument must be a positive integer"
    else
        cd $(printf "../%.0s" $(seq 1 $1))
    fi
}

function youtube-dl-1080p() {
    _filename=$(youtube-dl --get-filename ${1});
    youtube-dl -o 'a.m4a' -f 140 "${1}";
    youtube-dl -o 'v.mp4' -f 137 "${1}";
    ffmpeg -i "v.mp4" -i "a.m4a" \
        -c:v copy -c:a copy \
        "${_filename}" \
        && rm a.m4a v.mp4
}

function findext() {
    find . -name "*.$1"
}

function grepext() {
    findext $1 | xargs grep -In $2
}

