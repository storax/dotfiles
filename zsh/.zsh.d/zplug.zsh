#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# zplug

if [[ -z $ZPLUG_HOME ]]; then
    export ZPLUG_HOME=~/.zplug
fi

# Install zplug if it does not exist
[ -d $ZPLUG_HOME ] || source ~/.zsh.d/zplug/installer.zsh

source ~/.zplug/init.zsh

zplug "b4b4r07/zsh-vimode-visual", defer:3
zplug "zsh-users/zaw"
# zplug "oknowton/zsh-dwim"
zplug "plugins/colored-man-pages", from:oh-my-zsh
zplug "plugins/colorize", from:oh-my-zsh
zplug "plugins/command-not-found", from:oh-my-zsh
zplug "plugins/copydir", from:oh-my-zsh
zplug "plugins/copyfile", from:oh-my-zsh
zplug "plugins/cp", from:oh-my-zsh
zplug "plugins/dirhistory", from:oh-my-zsh
zplug "plugins/emacs", from:oh-my-zsh
zplug "plugins/gem", from:oh-my-zsh
zplug "plugins/git", from:oh-my-zsh
zplug "plugins/git-flow", from:oh-my-zsh
zplug "plugins/nyan", from:oh-my-zsh
zplug "plugins/pip", from:oh-my-zsh
zplug "plugins/python", from:oh-my-zsh
zplug "plugins/sudo", from:oh-my-zsh
zplug "plugins/vagrant", from:oh-my-zsh
zplug "rupa/z"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug "themes/kphoen", as:theme, from:oh-my-zsh

if ! zplug check; then
    zplug install
fi

zplug load
