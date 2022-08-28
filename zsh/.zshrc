# Set the theme.
ZSH_THEME="bla"

setopt extendedglob
unsetopt AUTO_CD

# Select zsh plugins.
plugins=(colored-man-pages cp git systemd z ssh-agent)
zstyle :omz:plugins:ssh-agent identities id_ed25519

source $ZSH/oh-my-zsh.sh

# terminfo
if [[ -e /usr/share/terminfo/x/xterm-256color ]]; then
  export TERM="xterm-256color"
fi

case $(uname -a) in
  *Microsoft*) unsetopt BG_NICE ;;
esac

# Aliases
alias autofs="sudo automount -fv"
alias bat="acpi"
alias booterrors="sudo journalctl -b -p err"
alias clang++11="clang++ -stdlib=libc++ -std=c++11 -lc++ -lc++abi"
alias clip="xclip -selection clipboard"
alias dmesg="sudo dmesg -T"
alias encoding="file -b --mime-encoding"
alias grep="grep -E --color=always"
alias jpg2pdf="convert -compress JPEG -quality 50"
alias l="ls -lF"
alias lo="libreoffice"
alias la="ls -lAF"
alias languagetool="GDK_SCALE=2 languagetool"
alias less="less -R"
alias lm="l -ctr"
alias ls="ls -h --color=always --group-directories-first"
alias mpv="mpv --x11-netwm=yes"
alias ocaml="rlwrap ocaml"
alias octmod="stat -c %a"
alias pcc="paccache -d -k2 -vvv"
alias pdf="evince"
alias pmpl="pacman -Qqe"
alias pmqi="pacman -Qi"
alias pmql="pacman -Ql"
alias pmqm="pacman -Qm"
alias pmqo="pacman -Qo"
alias pmqs="pacman -Qs"
alias pmrs="sudo pacman -Rs"
alias pms="sudo pacman -S"
alias pmsi="sudo pacman -Si"
alias pmss="pacman -Ss"
alias pmsyu="sudo pacman -Syu"
alias pmu="sudo pacman -U"
alias revparse="git rev-parse --short"
alias texmake="latexmk -pdf -pvc -interaction=nonstopmode"
alias valgrind="valgrind --leak-check=full"
alias vi="vim"
if [ "$(command -v nvim)" != "" ]; then
  alias vim="nvim"
fi
alias which="command -v"
alias yearn="journalctl"  # Do you ever yearn?
alias ytdl="yt-dlp -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best'"

# Functions
findext() {
  find . -name "*.$1"
}

grepext() {
  findext $1 | xargs grep -In $2
}

timer() {
  date_=$((`date +%s` + $1))
  while [ "$date_" -ne `date +%s` ]; do
    echo -ne "$(date -u --date @$(($date_ - `date +%s`)) +%H:%M:%S)\r";
    sleep 0.1
  done
  zenity --info --text="Time's up, jabroni!"
}

fname() {
  find . -iname "*$@*"
}

medialength() {
  echo "$1: $(ffprobe -i $1 -show_entries format=duration -v quiet -of \
              csv='p=0' -sexagesimal)"
}

mergesubs() {
  ffmpeg -i $1 -i $2 -c copy -metadata:s:s:0 language=eng $3
}

repair-pdf() {
  if [ $# -lt 2 ]; then
    echo "Two arguments required"
  else
    gs \
    -q \
    -o "$2" \
    -sDEVICE=pdfwrite \
    -dEmbedAllFonts=true \
    "$1"
  fi
}

mkpasswd() {
  if [ $# -eq 1 ]; then
    echo -n $(openssl rand -base64 $1)
  else
    echo -n $(openssl rand -base64 16)
  fi
}
