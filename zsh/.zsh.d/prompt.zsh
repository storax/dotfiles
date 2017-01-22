#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# Prompt
setopt prompt_subst # Enable parameter expansion, command substitution, and arithmetic expansion in the prompt
#setopt transient_rprompt # only show the rprompt on the current prompt

autoload -U colors && colors # Enable colors in prompt

function virtualenv_info {
    [ $VIRTUAL_ENV ] && echo ' |'`basename $VIRTUAL_ENV`'|'
}

# Show Git branch/tag, or name-rev if on detached head
function parse_git_branch() {
    (git symbolic-ref -q HEAD || git name-rev --name-only --no-undefined --always HEAD) 2> /dev/null
}

git_prompt_info () {
	  if [[ "$(command git config --get oh-my-zsh.hide-status 2>/dev/null)" != "1" ]]
	  then
		    ref=$(parse_git_branch) || return 0
		    echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#(refs/heads/|tags/)}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
	  fi
}

export SPROMPT="Correct $fg[red]%R$reset_color to $fg[green]%r$reset_color [(y)es (n)o (a)bort (e)dit]? "

export PROMPT='[%{$fg[red]%}%n%{$reset_color%}@%{$fg[magenta]%}%m%{$reset_color%}:%{$fg[blue]%}%~%{$reset_color%}$(git_prompt_info)%{$fg[cyan]%}$(virtualenv_info)%{$reset_color%}]
%# '

precmd() {
    RPROMPT=""
}
zle-keymap-select() {
    RPROMPT=""
    [[ $KEYMAP = vicmd ]] && RPROMPT="%{$fg[yellow]%}-- NORMAL --%{$reset_color%}"
    () { return $__prompt_status }
    zle reset-prompt
}
zle-line-init() {
    typeset -g __prompt_status="$?"
}
zle -N zle-keymap-select
zle -N zle-line-init
