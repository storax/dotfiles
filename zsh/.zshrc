#  ███████╗███████╗██╗  ██╗██████╗  ██████╗
#  ╚══███╔╝██╔════╝██║  ██║██╔══██╗██╔════╝
#    ███╔╝ ███████╗███████║██████╔╝██║
#   ███╔╝  ╚════██║██╔══██║██╔══██╗██║
#  ███████╗███████║██║  ██║██║  ██║╚██████╗
#  ╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝

if [[ "$ZPROF" = true ]]; then
    zmodload zsh/zprof
fi

typeset -a sources
CONFIG_DIR="$HOME/.zsh.d"

sources+="$CONFIG_DIR/environment.zsh"
sources+="$CONFIG_DIR/setopt.zsh"
sources+="$CONFIG_DIR/functions.zsh"
sources+="$CONFIG_DIR/k8s.zsh"
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
sources+="$CONFIG_DIR/vim.zsh"

for file in $sources[@]; do
    if [[ -a $file ]]; then
       source $file
    else
        echo "config file not found: $file"
    fi
done

if [[ "$ZPROF" = true ]]; then
    zprof
fi
