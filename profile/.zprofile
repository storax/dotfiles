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

# if [ "$0" = "/usr/sbin/lightdm-session" -a "$DESKTOP_SESSION" = "i3" ]; then
#     export $(gnome-keyring-daemon -s)
# fi

# Has to be in .profile for i3
export TERMINAL=urxvt
