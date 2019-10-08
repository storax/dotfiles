#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# Misc. Environment Variables
export VIRTUAL_ENV_DISABLE_PROMPT=1
export GO111MODULE=on
export GOPATH=$HOME/go
export PATH="$HOME/.cask/bin:${GOPATH//://bin:}/bin:$PATH"
