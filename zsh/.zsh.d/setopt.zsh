#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# Misc. Options

# ===== Changing Directories
setopt auto_cd # If you type foo, and it isn't a command, and it is a directory in your cdpath, go there

# ===== Expansion and Globbing
setopt extended_glob # treat #, ~, and ^ as part of patterns for filename generation

# All unquoted arguments of the form 'anything=expression'
# appearing after the command name have filename expansion
# (that is, where expression has a leading '~' or '=')
# performed on expression as if it were a parameter assignment.
setopt magic_equal_subst

# ===== Scripts and Functions
setopt multios # perform implicit tees or cats when multiple redirections are attempted

