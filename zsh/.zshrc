# Set the theme.
ZSH_THEME="bla"

unsetopt AUTO_CD

# Select zsh plugins.
plugins=(colored-man cp git systemd z)

source $ZSH/oh-my-zsh.sh

# terminfo
if [[ -e /usr/share/terminfo/x/xterm-256color ]]; then
  export TERM="xterm-256color"
fi

# Aliases
alias avidemux="avidemux2_gtk"
alias clang++11="clang++ -stdlib=libc++ -std=c++11 -lc++ -lc++abi"
alias cleanpdf="ps2pdf -dPDFSETTINGS=/prepress"
alias clip="xclip -selection clipboard"
alias dmesg="dmesg -T"
alias encoding="file -b --mime-encoding"
alias grep="grep -E --color=always"
alias grepi="grep -i"
alias l="ls -lF"
alias la="ls -lAF"
alias less="less -R"
alias lm="l -ctr"
alias ls="ls -h --color=always --group-directories-first"
alias matlab="matlab -nojvm -nodesktop"
alias matlabgui="wmname LG3D; \matlab"
alias maxima="rlwrap rmaxima"
alias mkpasswd="openssl rand -base64 16"
alias octave="octave --no-gui -q"
alias octmod="stat -c %a"
alias pcc="paccache -d -k2 -vvv"
alias pdf="evince"
alias pep8="pep8-python2"
alias pmpl="pacman -Qqe"
alias pmqi="pacman -Qi"
alias pmql="pacman -Ql"
alias pmqo="pacman -Qo"
alias pmqs="pacman -Qs"
alias pmr="sudo pacman -R"
alias pmrs="sudo pacman -Rs"
alias pms="sudo pacman -S"
alias pmsi="sudo pacman -Si"
alias pmss="pacman -Ss"
alias pmsyu="sudo pacman -Syu"
alias pmu="sudo pacman -U"
alias revparse="git rev-parse --short"
alias sinfo="ssh -t vader -- sinfo"
alias texmake="latexmk -pdf -pvc -interaction=nonstopmode"
alias valgrind="valgrind --leak-check=full"
alias vi="vim"
if [ "$(command -v nvim)" != "" ]; then
  alias vim="nvim"
fi
alias which="command -v"
alias yearn="journalctl" # Do you ever yearn?

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
  zenity --info --text="Time's up, jabroni!"
}

twitch() {
  livestreamer twitch.tv/$1 best
}

clonebitbucket() {
  git clone git@bitbucket.org:nkoep/$1.git $2
}

psgrep() {
  ps axuf | grep -v grep | grep "$@" -i --color=auto
}

fname() {
  find . -iname "*$@*"
}

medialength() {
  echo "$1: $(ffprobe -i $1 -show_entries format=duration -v quiet -of \
              csv='p=0' -sexagesimal)"
}

_bat() {
  echo BAT$1: $(upower -i /org/freedesktop/UPower/devices/battery_BAT$1 | \
                grep -E "state|to\ full|percentage")
}
bat() {
  if [ "$(upower --enumerate | grep BAT)" != "" ]; then
    _bat 0
    _bat 1
  else
    echo "No battery found"
  fi
}
