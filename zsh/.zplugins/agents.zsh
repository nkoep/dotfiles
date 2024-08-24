# SSH agent.
if command -v ssh-agent >/dev/null; then
  ssh_env_path="$XDG_RUNTIME_DIR/ssh-agent.env"
  if ! pgrep -u "$USER" ssh-agent >/dev/null; then
    ssh-agent >"$ssh_env_path"
  fi
  if [[ ! -f "$SSH_AUTH_SOCK" ]]; then
    source "$ssh_env_path" >/dev/null
  fi
fi

# GPG agent.
if command -v gpg-agent >/dev/null; then
  if ! pgrep -u "$USER" gpg-agent >/dev/null; then
    eval "$(gpg-agent --daemon --write-env-file "$HOME/.gpg-agent-info")"
  fi
  if [[ -f "$HOME/.gpg-agent-info" ]]; then
    source "$HOME/.gpg-agent-info"
    export GPG_AGENT_INFO
  fi
  if [[ -n "$GPG_AGENT_INFO" ]]; then
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
  fi
fi
