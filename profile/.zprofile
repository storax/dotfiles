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

if [ -z $SSH_AGENT_PID ] && [ -z $SSH_TTY ]; then  # if no agent & not in ssh
    eval `ssh-agent -s` > /dev/null
fi
# if [ "$0" = "/usr/sbin/lightdm-session" -a "$DESKTOP_SESSION" = "i3" ]; then
#     export $(gnome-keyring-daemon -s)
# fi

# Has to be in .profile for i3
export TERMINAL="urxvt"
export EDITOR=vim
export VISUAL=vim

if [ -z "$DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
    exec startx
fi
