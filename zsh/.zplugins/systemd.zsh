user_commands=(
  cat
  help
  is-active
  is-enabled
  is-failed
  list-dependencies
  list-unit-files
  list-units
  show
  status
)
sudo_commands=(
  daemon-reload
  disable
  enable
  restart
  start
  stop
)
power_commands=(
  poweroff
  reboot
)

for c in $user_commands; do
  alias "sc-$c"="systemctl $c"
  alias "scu-$c"="systemctl --user $c"
done

for c in $sudo_commands; do
  alias "sc-$c"="sudo systemctl $c"
  alias "scu-$c"="systemctl --user $c"
done

for c in $power_commands; do
  alias "sc-$c"="systemctl $c"
done

alias sc-enable-now="sc-enable --now"
alias scu-enable-now="scu-enable --now"
alias sc-disable-now="sc-disable --now"
alias scu-disable-now="scu-disable --now"

unset c user_commands sudo_commands power_commands
