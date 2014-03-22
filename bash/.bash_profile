#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

