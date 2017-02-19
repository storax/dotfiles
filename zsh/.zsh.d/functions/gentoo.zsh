#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# Gentoo Functions

kernelprep() {
    # check if boot is mounted
    mount | grep -q /boot
    if [ $? -eq 1 ]; then
        sudo mount /boot
    fi
    cd /usr/src/linux
}

kernelmake() {
    kernelprep
    sudo make && sudo make modules_install && sudo make install
    sudo umount /boot
}