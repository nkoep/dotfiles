local editor="nvim"

export BROWSER="firefox"
export EDITOR="$editor"
export GPG_TTY=$(tty)
export LESS="-R "
export MKL_THREADING_LAYER=GNU
export PI_MODELS="deepseek-v4-pro,claude-sonnet-4.6,gemini-3.1-flash-lite-preview,gemini-3-flash-preview,gemini-3.1-pro-preview"
export PI_PROVIDER="openrouter,google"
export PI_SKIP_VERSION_CHECK=1
export PI_TOOLS="read,bash,edit,write,grep,find,ls"
export TERM=xterm-256color
export TEXMFHOME="$HOME/.texmf"
export VISUAL="$editor"

if [[ -x /usr/bin/lesspipe ]]; then
  eval "$(lesspipe)"
elif [[ -x /usr/bin/lesspipe.sh ]]; then
  eval "$(lesspipe.sh)"
fi

if [[ -e ~/.zshenv_keys ]]; then
  source ~/.zshenv_keys
fi

if [[ -e ~/.work/zshenv ]]; then
  source ~/.work/zshenv
fi
