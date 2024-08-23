autoload -U colors && colors
typeset -AHg less_termcap

# Bold & blinking mode
less_termcap[mb]="${fg_bold[red]}"
less_termcap[md]="${fg_bold[red]}"
less_termcap[me]="${reset_color}"
# Standout mode
less_termcap[so]="${fg_bold[yellow]}${bg[blue]}"
less_termcap[se]="${reset_color}"
# Underlining
less_termcap[us]="${fg_bold[green]}"
less_termcap[ue]="${reset_color}"

function colored() {
  local -a environment
  for k v in "${(@kv)less_termcap}"; do
    environment+=( "LESS_TERMCAP_${k}=${v}" )
  done
  environment+=( PAGER="${commands[less]:-$PAGER}" )
  environment+=( GROFF_NO_SGR=1 )
  command env $environment "$@"
}

# function man {
#   colored command man "$@"
# }
function man {
  colored $0 "$@"
}
