#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# Tmux shell config

[ -d ~/software/tmuxifier ] || git clone https://github.com/jimeh/tmuxifier.git ~/software/tmuxifier
export PATH="$HOME/software/tmuxifier/bin:$PATH"
export TMUXIFIER_LAYOUT_PATH="$HOME/.config/tmux/tmux-layouts"
eval "$(tmuxifier init -)"
