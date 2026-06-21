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
      -dCompatibilityLevel=1.4 \
      -dPDFSETTINGS=/ebook \
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
  if [ $# -ne 2 ]; then
    echo "Use: $0 <input> <output>"
  else
    gpg -r $USER -e -o "$2" "$1"
  fi
}

gpg-encrypt-pass() {
  if [ $# -eq 1 ]; then
    input="$1"
    output="${1}.gpg"
  elif [ $# -eq 2 ]; then
    input="$1"
    output="$2"
  else
    echo "One or two arguments required"
    return 1
  fi
  gpg --symmetric --cipher-algo AES256 -o "$output" "$input"
}

gpg-decrypt() {
  if [ $# -ne 2 ]; then
    echo "Use: $0 <input> <output>"
  else
    gpg -d -o "$2" "$1"
  fi
}

clean-caches() {
  echo "Cleaning pacman cache:"
  paccache -dk1
  echo "(Installed packages)"
  paccache -duk0
  echo "(Uninstalled packages)"
  if read -q "choice?Proceed? Y/n"; then
    paccache -rk1
    paccache -ruk0
  fi
}

py() {
  echo "$1" >.python-version
}

md2html() {
  if [ $# -ne 2 ]; then
    echo "Use: $0 <input> <output>"
  else
    pandoc \
      "$1" \
      -s \
      --toc \
      --mathjax \
      --syntax-highlight pygments \
      -c https://cdn.jsdelivr.net/npm/water.css@2/out/water.css \
      -o "$2"
  fi
}

clip() {
  local input
  input="$(cat | sed -E $'s/\x1b\\[[0-9;]*[a-zA-Z]//g')"
  if [[ "$(uname)" == "Darwin" ]]; then
    printf '%s' "$input" | pbcopy
  elif [ -n "$WAYLAND_DISPLAY" ]; then
    wl-copy --trim-newline <<<"$input"
  else
    printf '%s' "$input" | xclip -selection clipboard
  fi
}
