#  ██████╗ ██████╗  ██████╗ ███████╗██╗██╗     ███████╗
#  ██╔══██╗██╔══██╗██╔═══██╗██╔════╝██║██║     ██╔════╝
#  ██████╔╝██████╔╝██║   ██║█████╗  ██║██║     █████╗
#  ██╔═══╝ ██╔══██╗██║   ██║██╔══╝  ██║██║     ██╔══╝
#  ██║     ██║  ██║╚██████╔╝██║     ██║███████╗███████╗
#  ╚═╝     ╚═╝  ╚═╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝╚══════╝
# profile

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$HOME/.local/bin:$PATH"
fi

eval `ssh-agent`
ssh-add ~/.ssh/id_github4096_rsa
# if [ "$0" = "/usr/sbin/lightdm-session" -a "$DESKTOP_SESSION" = "i3" ]; then
#     export $(gnome-keyring-daemon -s)
# fi

# Has to be in .profile for i3
export TERMINAL=urxvt
export EDITOR=vim
export VISUAL=vim
