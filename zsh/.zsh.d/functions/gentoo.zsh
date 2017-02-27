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
    sudo make -j7 modules_prepare
    sudo emerge --ask @module-rebuild
    sudo make -j7 && sudo make -j7 modules_install && sudo make -j7 install
    sudo grub-mkconfig -o /boot/grub/grub.cfg
}

kernelclean() {
    sudo emerge --ask --depclean gentoo-sources
    echo "Run: eclean-kernel -n X"
}
