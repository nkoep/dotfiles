# Expand variables and commands in PROMPT variables
setopt prompt_subst

function branch_name() {
  branch="$(git branch --show-current 2>/dev/null)"
  if [[ -n "$branch" ]]; then
    echo "⚡ %B$branch%b "
  fi
}

function venv() {
  if [[ -n "$VIRTUAL_ENV" ]]; then
    python_version="$(python -V | awk '{print $2}')"
    echo " ${python_version} | ${VIRTUAL_ENV##*/}"
  fi
}

local return_value="%(?:%{$fg_bold[green]%}»:%{$fg_bold[red]%}»%s)"
PROMPT='%~ $(branch_name)${return_value}%{$reset_color%} '
export RPROMPT='$(venv)'
