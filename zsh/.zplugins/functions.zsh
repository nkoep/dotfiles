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

py() {
  echo "$1" >.python-version
}

aur() {
  if [[ -z "$1" ]]; then
    echo "Usage: aur_install <package_name>"
    return 1
  fi

  package="$1"
  temp_dir=$(mktemp -d)

  cd "$temp_dir" || return 1

  if ! git clone "https://aur.archlinux.org/${package}.git"; then
    echo "Error: Failed to clone AUR repository for $package."
    rm -rf "$temp_dir"
    return 1
  fi

  cd "$package" || return 1

  if ! makepkg -si --noconfirm; then
    echo "Error: Failed to build and install $package."
    rm -rf "$temp_dir"
    return 1
  fi

  rm -rf "$temp_dir"
}
