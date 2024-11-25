alias autofs="sudo automount -fv"
alias bat="bat --paging=always"
alias clip="xclip -selection clipboard"
alias dmesg="sudo dmesg -T"
alias encoding="file -b --mime-encoding"
alias gc="git commit -s"
alias gd="git diff"
alias grep="grep -E --color=always"
alias jpg2pdf="convert -compress JPEG -quality 50"
alias l="ls -lF"
alias la="ls -lAF"
alias less="less -R"
alias lm="l -ctr"
alias lo="libreoffice"
alias ls="ls -h --color=always --group-directories-first"
alias lt="lz --tree"
alias lz="eza -lF --group-directories-first --icons"
alias mpv="mpv --x11-netwm=yes"
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
alias pmsy="sudo pacman -Sy"
alias pmsyu="sudo pacman -Syu"
alias pmu="sudo pacman -U"
alias pq="python -q"
alias tarit='function _tarit() { tar -czvf "$1.tar.gz" "$1"; }; _tarit'
alias texmake="latexmk -pdf -pvc -interaction=nonstopmode"
alias vim="nvim"
alias weather="curl -s wttr.in/Charlottenburg | grep -v 'Follow'"
alias which="command -v"
alias yt="mpv --ytdl-raw-options'=format-sort=\"res:1080,vcodec:h264,acodec:m4a\"'"
alias ytdl="yt-dlp -S \"res:1080,vcodec:h264,acodec:m4a\""
