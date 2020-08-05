#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# vim

SPACEVIM_HOME=~/.SpaceVim
INSTALLER_URL=https://spacevim.org/install.sh
TMP_INSTALLER=/tmp/spacevim_install.sh
EXPECTED_CHECKSUM="6038ab8808da613af48079ab8f8396b6666080478ee9c2d0ecb4c98e31eef87ad593e1724d2c80acdea733bb68174ec46c7106f628f0e45187325a8597f4764d  $TMP_INSTALLER"

install_spacevim() {
    echo "Installing SpaceVim"
    curl -sLf $INSTALLER_URL > $TMP_INSTALLER
    if [ ! $? -eq 0 ]; then
        echo "Downloading installer $INSTALLER_URL failed."
        return 1
    fi

    checksum=$(sha512sum $TMP_INSTALLER)
    if [ ! "$checksum" = "$EXPECTED_CHECKSUM" ]; then
        echo "$TMP_INSTALLER has wrong checksum"
        echo "$checksum"
        echo "Expected:"
        echo "$EXPECTED_CHECKSUM"
        rm -f $TMP_INSTALLER
        return 1
    fi
    chmod +x $TMP_INSTALLER

    $TMP_INSTALLER

    rm -f $TMP_INSTALLER
    echo "SpaceVim installed"
}

if [ ! -d $SPACEVIM_HOME ]; then
    install_spacevim
fi
