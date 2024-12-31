findext() {
  find . -name "*.$1"
}

grepext() {
  findext $1 | xargs grep -In $2
}

findname() {
  find . -iname "*$@*"
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

gpg-encrypt-pass() {
  if [ $# -ne 1 ]; then
    input="$1"
    output="${1}.gpg"
  else
    input="$1"
    output="$2"
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

mise-venv() {
  python_version="$1"
  venv_path=".venv_py${python_version}"
  mise install "python@${python_version}" || {
    echo "Failed to install python version"
    return 1
  }
  cat <<EOF >.mise.toml
[tools]
python = "$python_version"

[env]
_.python.venv = { path = "$venv_path", create = true }
EOF
  mise trust
}