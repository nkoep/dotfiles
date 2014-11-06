# Start the SSH user agent.
if [[ -e $(which gnome-keyring) ]]; then
    eval $(gnome-keyring-daemon --start)
    export SSH_AUTH_SOCK
fi

