#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# Alias

alias xo=xdg-open
alias wttr='curl wttr.in/london'
alias p="ps -ef"
alias -g G="| grep -i"
alias dfe="df -ht ext4"
alias zzz="sudo shutdown -P +60"
alias cya="sudo shutdown -P now"
# -------------------------------------------------------------------
# use nocorrect alias to prevent auto correct from "fixing" these
# -------------------------------------------------------------------
#alias foobar='nocorrect foobar'

# -------------------------------------------------------------------
# directory movement
# -------------------------------------------------------------------
alias 'bk=cd $OLDPWD'

alias 'dus=du -sckx * | sort -n' #directories sorted by size

alias 'wordy=wc -w * | sort | tail -n10' # sort files in current directory by the number of words they contain
alias 'filecount=find . -type f | wc -l' # number of files (not directories)

# -------------------------------------------------------------------
# Git
# -------------------------------------------------------------------
# curiosities
# gsh shows the number of commits for the current repos for all developers
alias gsh="git shortlog | grep -E '^[ ]+\w+' | wc -l"

# gu shows a list of all developers and the number of commits they've made
alias gu="git shortlog | grep -E '^[^ ]'"

# -------------------------------------------------------------------
# Python virtualenv
# -------------------------------------------------------------------
alias mkenv='mkvirtualenv'
alias on="workon"
alias off="deactivate"

# Force tmux to use 256 colors
alias tmux='TERM=screen-256color-bce tmux'

# alias to cat this file to display
alias acat='< ~/.zsh.d/alias.zsh'
alias fcat='< ~/.zsh.d/functions.zsh'
alias sz='source ~/.zshrc'

# -------------------------------------------------------------------
# Source: http://aur.archlinux.org/packages/lolbash/lolbash/lolbash.sh
# -------------------------------------------------------------------
alias onoz='cat /var/log/errors.log'
alias rtfm='man'
alias visible='echo'
alias invisible='cat'
alias moar='more'
alias icanhas='mkdir'
alias donotwant='rm'
alias dowant='cp'
alias gtfo='mv'
alias hai='cd'
alias plz='pwd'
alias inur='locate'
alias nomz='ps aux | less'
alias nomnom='killall'
alias cya='reboot'
alias kthxbai='halt'
