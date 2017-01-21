#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# zaw

####################################
# better alias source
####################################
function zaw-src-aliases() {
    title="aliases"
    desc="$(alias | zaw-src-aliases-desc)"
    units="$(alias | cut -d '=' -f 1)"
    : ${(A)candidates::=${(f)units}}
    : ${(A)cand_descriptions::=${(f)desc}}
    actions=(zaw-callback-execute zaw-callback-append-to-buffer zaw-callback-replace-buffer view-alias-pager)
    act_descriptions=("execute" "append to buffer" "replace buffer" "view alias")
    options=(-t "$title")
}

zaw-src-aliases-max() {
    alias | awk -F'=' '{max=(length($1)>max?length($1):max)} END {print max}'
}

zaw-src-aliases-desc() {
    max=`alias | awk -F'=' '{max=(length($1)>max?length($1):max)} END {print max}'`
    awk -F'=' -v max=$max 'BEGIN {format = "%-" max "s = "} {printf (format, $1); for (i=2; i<NF; i++) printf $i "="; print $NF}'
}

view-alias-pager() {
    alias $1 | ${PAGER:-less}
}

zaw-register-src -n aliases zaw-src-aliases

####################################
# z source
# credits to NigoroJr/zaw-z from where i ripped this off.
####################################
# Just like zaw-callback-execute but with cd
zaw-callback-cd() {
    # Substitute first \~ to ~
    local dest=${${(q)1}/#\\~/\~}
    BUFFER="$ZAW_Z_CD_CMD $dest"
    zle accept-line
}

zaw-callback-remove-entry() {
    local dest=${~1}
    local z_cmd=${_Z_CMD:-z}
    ( cd $dest && eval "$z_cmd -x" >/dev/null )
}

zaw-src-z() {
    local z_cmd=${_Z_CMD:-z}

    IFS=$'\n' candidates=( $( eval "$z_cmd" | awk '{
        for (i = 2; i <= NF; i++) printf "%s", $i (i == NF ? ORS : OFS)
    }' | sed -e "s#$HOME#~#" ) )
    actions=( \
        zaw-callback-cd \
        zaw-callback-append-to-buffer \
        zaw-callback-replace-buffer \
        zaw-callback-remove-entry \
        )
    act_descriptions=( \
        'cd into the selected directory' \
        'append to buffer' \
        'replace current line of buffer' \
        'remove entry from z database' \
        )
}

zaw-register-src -n 'z' zaw-src-z
