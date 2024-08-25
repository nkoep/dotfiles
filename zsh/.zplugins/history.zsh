# History file configuration
[ -z "$HISTFILE" ] && HISTFILE="$HOME/.zsh_history"
[ "$HISTSIZE" -lt 50000 ] && HISTSIZE=50000
[ "$SAVEHIST" -lt 10000 ] && SAVEHIST=10000

# History command configuration
setopt extended_history        # Record timestamp of command in HISTFILE.
setopt hist_expire_dups_first  # Delete duplicates first when buffer exhausted.
setopt hist_find_no_dups       # Do not display a previously found event.
setopt hist_ignore_all_dups    # Delete old event if new event is a duplicate.
setopt hist_ignore_space       # Ignore commands that start with space.
setopt hist_verify             # Show command with history expansion.
setopt share_history           # Share command history data.
