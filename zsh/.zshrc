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
alias csh="configure-screens home"
alias csw="configure-screens work"
alias lm="l -ictr"
alias clip="xclip -sel clip"
alias pdf="evince"
alias avidemux="avidemux2_gtk"
alias texmake="latexmk -pdf -pvc -interaction=nonstopmode"
alias vader="ssh vader"

# Functions
cdup() {
  if ! [[ $1 =~ [0-9]+ && $1 -gt 0 ]]; then
    echo "argument must be a positive integer"
  else
    cd $(printf "../%.0s" $(seq 1 $1))
  fi
}

youtube-dl-1080p() {
  _filename=$(youtube-dl --get-filename ${1});
  youtube-dl -o 'a.m4a' -f 140 "${1}";
  youtube-dl -o 'v.mp4' -f 137 "${1}";
  ffmpeg -i "v.mp4" -i "a.m4a" \
    -c:v copy -c:a copy \
    "${_filename}" \
    && rm a.m4a v.mp4
}

findext() {
  find . -name "*.$1"
}

grepext() {
  findext $1 | xargs grep -In $2
}

wiki() {
  url="http://en.wikipedia.org/w/api.php?continue=&action=query&"\
"prop=extracts&exintro=&explaintext=&format=json&redirects"
  curl -s -G $url --data-urlencode titles="$*" | jq \
    -r ".query.pages[].extract" | fold -s -w 80
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

