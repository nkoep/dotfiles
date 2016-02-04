# Set the theme.
ZSH_THEME="bla"

# Select zsh plugins.
plugins=(colored-man cp git systemd z)

source $ZSH/oh-my-zsh.sh

# terminfo
if [[ -e /usr/share/terminfo/x/xterm-256color ]]; then
  export TERM="xterm-256color"
fi

# Aliases
alias clang++11="clang++ -stdlib=libc++ -std=c++11 -lc++ -lc++abi"
alias less="less -R"
alias ls="ls -h --color=always --group-directories-first"
alias l="ls -lF"
alias la="ls -lAF"
alias grep="grep -E"
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
alias pmr="sudo pacman -R"
alias pmrs="sudo pacman -Rs"
alias pmpl="pacman -Qqe"
alias pcc="paccache -d -k2 -vvv"
alias python="python -q"
alias valgrind="valgrind --leak-check=full"
alias octmod="stat -c %a"
alias revparse="git rev-parse --short"
alias encoding="file -b --mime-encoding"
alias yearn="journalctl" # Do you ever yearn?
alias lm="l -ictr"
alias clip="xclip -sel clip"
alias pdf="evince"
alias avidemux="avidemux2_gtk"
alias texmake="latexmk -pdf -pvc -interaction=nonstopmode"
alias cleanpdf="ps2pdf -dPDFSETTINGS=/prepress"
alias maxima="rlwrap rmaxima"
alias dmesg="dmesg -T"
alias vi="vim"
alias sinfo="ssh -t vader -- sinfo"
alias octave="octave --no-gui -q"
alias matlab="matlab -nojvm -nodesktop"
alias pep8="pep8-python2"

# Functions
findext() {
  find . -name "*.$1"
}

grepext() {
  findext $1 | xargs grep -In $2
}

timer() {
  date_=$((`date +%s` + $1));
  while [ "$date_" -ne `date +%s` ]; do
    echo -ne "$(date -u --date @$(($date_ - `date +%s`)) +%H:%M:%S)\r";
    sleep 0.1
  done
  zenity --info --text="Aaaaand time!"
}

twitch() {
  livestreamer twitch.tv/$1 best
}

