#  ███████╗███████╗██╗  ██╗██████╗  ██████╗
#  ╚══███╔╝██╔════╝██║  ██║██╔══██╗██╔════╝
#    ███╔╝ ███████╗███████║██████╔╝██║
#   ███╔╝  ╚════██║██╔══██║██╔══██╗██║
#  ███████╗███████║██║  ██║██║  ██║╚██████╗
#  ╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝

# for profiling uncomment line below
#zmodload zsh/zprof
# then execute zprof for analysis after startup

typeset -a sources
CONFIG_DIR="$HOME/.zsh.d"

sources+="$CONFIG_DIR/environment.zsh"
sources+="$CONFIG_DIR/setopt.zsh"
sources+="$CONFIG_DIR/functions.zsh"
sources+="$CONFIG_DIR/completion.zsh"
sources+="$CONFIG_DIR/zplug.zsh"
sources+="$CONFIG_DIR/zaw.zsh"
sources+="$CONFIG_DIR/alias.zsh"
sources+="$CONFIG_DIR/bindkey.zsh"
sources+="$CONFIG_DIR/colors.zsh"
sources+="$CONFIG_DIR/history.zsh"
sources+="$CONFIG_DIR/prompt.zsh"
sources+="$CONFIG_DIR/pyenv.zsh"
sources+="$CONFIG_DIR/tmux.zsh"

for file in $sources[@]; do
    if [[ -a $file ]]; then
       source $file
    else
        echo "config file not found: $file"
    fi
done
