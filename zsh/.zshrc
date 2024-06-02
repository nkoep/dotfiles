# Set the theme.
ZSH_THEME="bla"

setopt extendedglob
unsetopt AUTO_CD

# Select zsh plugins.
plugins=(
  colored-man-pages
  cp
  git
  gpg-agent
  python
  ssh-agent
  systemd
  z
)

typeset -U identities
for identity in id_rsa id_ed25519; do
  if [ -f ~/.ssh/${identity} ]; then
    identities+=($identity)
  fi
done
zstyle :omz:plugins:ssh-agent identities $identities

source $ZSH/oh-my-zsh.sh

# Source pyenv.
export PYENV_ROOT="$HOME/.pyenv"
path+=("$PYENV_ROOT/bin")
if command -v pyenv >/dev/null; then
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

# Aliases
alias autofs="sudo automount -fv"
alias booterrors="sudo journalctl -b -p err"
alias clip="xclip -selection clipboard"
alias dmesg="sudo dmesg -T"
alias encoding="file -b --mime-encoding"
alias grep="grep -E --color=always"
alias jpg2pdf="convert -compress JPEG -quality 50"
alias l="ls -lF"
alias la="ls -lAF"
alias languagetool="GDK_SCALE=2 languagetool"
alias less="less -R"
alias lm="l -ctr"
alias lo="libreoffice"
alias ls="ls -h --color=always --group-directories-first"
alias mpv="mpv --x11-netwm=yes"
alias ocaml="rlwrap ocaml"
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
alias pq="python -q"
alias revparse="git rev-parse --short"
alias texmake="latexmk -pdf -pvc -interaction=nonstopmode"
alias vim="nvim"
alias which="command -v"
alias xrc="xmonad --recompile"
alias yearn="journalctl"  # Do you ever yearn?
alias ytdl="yt-dlp -S 'res:1080,vcodec:h264,acodec:m4a'"

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

gpg-encrypt() {
  if [ $# -eq 1 ]; then
    gpg -r $USER -e -o "$1.gpg" "$1"
  else
    gpg -r $USER -e -o "$2" "$1"
  fi
}

gpg-decrypt() {
  if [ $# -ne 2 ]; then
    echo "Use: $0 <input> <output>"
  else
    gpg -d -o "$2" "$1"
  fi
}

send-ses-email() {
  from="$1"
  to="$2"
  subject="$3"
  body="$4"
  aws ses \
    send-email \
    --region eu-west-1 \
    --from "$from" \
    --to "$to" \
    --message "Subject={Data=\"$subject\",Charset=\"UTF-8\"},Body={Html={Data=\"$body\",Charset=\"UTF-8\"}}"
}

clean-caches() {
  echo "Cleaning pacman cache:"
  paccache -dk1
  echo "(Installed packages)"
  paccache -duk0
  echo "(Uninstalled packages)"
  if read -q "choice?Proceed? Y/n"; then
    paccache -rk1; paccache -ruk0;
  fi

  echo "Cleaning pacaur cache:"
  pacaur --aur-cleanall

  echo "Cleaning pip cache:"
  pip cache purge

  echo "Cleaning pipenv cache:"
  pipenv --clear

  echo "Cleaning yarn cache:"
  yarn cache clean
}

if [ -e ~/.work/zshrc ]; then
  source ~/.work/zshrc
fi
