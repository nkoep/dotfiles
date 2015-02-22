setopt prompt_subst
autoload -U colors && colors

function branch_name() {
    branch=$(current_branch)
    if [[ -n $branch ]]; then
        echo "(%B$branch%b) "
    fi
}

local return_value="%(?:%{$fg_bold[green]%}»:%{$fg_bold[red]%}»%s)"
PROMPT='[%m ⚡ %~] $(branch_name)${return_value}%{$reset_color%} '

