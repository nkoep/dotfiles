# TODO(nkoep): Migrate ssh keys to GPG, and remove SSH agent.
if command -v ssh-agent >/dev/null; then
  ssh_agent_env="$HOME/.ssh/ssh_agent.env"
  if ! pgrep -u "$USER" ssh-agent >/dev/null; then
    ssh-agent >"$ssh_agent_env"
  fi
  if [[ ! -f "$SSH_AUTH_SOCK" ]]; then
    source "$ssh_agent_env" >/dev/null
  fi
fi
