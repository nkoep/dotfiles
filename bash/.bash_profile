#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

eval $(ssh-agent)
ssh-add -l >/dev/null ||
alias ssh="ssh-add -l >/dev/null || ssh-add && unalias ssh; ssh"

