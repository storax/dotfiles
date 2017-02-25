#  ███████╗███████╗██╗  ██╗
#  ╚══███╔╝██╔════╝██║  ██║
#    ███╔╝ ███████╗███████║
#   ███╔╝  ╚════██║██╔══██║
#  ███████╗███████║██║  ██║
#  ╚══════╝╚══════╝╚═╝  ╚═╝
# pyenv

# Download pyenv if it is not there yet
[ -d ~/.pyenv/bin ] || git clone https://github.com/yyuu/pyenv.git ~/.pyenv
[ -d ~/.pyenv/plugins/pyenv-virtualenv ] || git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
[ -d ~/.pyenv/plugins/pyenv-virtualenvwrapper ] || git clone https://github.com/yyuu/pyenv-virtualenvwrapper.git ~/.pyenv/plugins/pyenv-virtualenvwrapper

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/projects/
source /usr/bin/virtualenvwrapper_lazy.sh

# build python with enable shared
export PYTHON_CONFIGURE_OPTS="--enable-unicode=ucs4 --enable-shared"
export PATH="$HOME/.pyenv/bin:$PATH"

pyi () {
    eval "$(pyenv init -)"
    pyenv virtualenvwrapper_lazy
}
