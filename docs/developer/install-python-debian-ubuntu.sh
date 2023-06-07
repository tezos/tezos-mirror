#!/bin/bash

usage () {
    cat >&2 <<!EOF
usage:
  $0 [<repo> <branch>]
!EOF
}

if [ $# -eq 2 ];
then
  REPO=$1
  BRANCH=$2
elif [ $# -eq 0 ]
then
  REPO="tezos/tezos"
  BRANCH="latest-release"
else
  usage
  exit 1
fi

set -e
set -x

# Non-python-related setup, to make this script read more like the
# installation from the point of view of a non-root user using sudo.
export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install sudo --yes
echo 'Defaults env_keep += "DEBIAN_FRONTEND"' >> /etc/sudoers

##
## Install pyenv
##
## References:
##  - https://github.com/pyenv/pyenv/wiki#suggested-build-environment
##  - https://github.com/pyenv/pyenv-installer
##  - https://github.com/pyenv/pyenv/#set-up-your-shell-environment-for-pyenv

# [install pyenv system dependencies]
sudo apt-get install curl git --yes

# [install python build dependencies]
sudo apt-get install make build-essential libssl-dev zlib1g-dev \
    libbz2-dev libreadline-dev libsqlite3-dev wget curl llvm \
    libncursesw5-dev xz-utils tk-dev libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev --yes

# [install pyenv]
curl https://pyenv.run | bash

# [setup shell for pyenv]
export PATH="$HOME/.pyenv/bin:$PATH" # add pyenv to path
eval "$(pyenv init --path)" # adds pyenv plugins to path
eval "$(pyenv init -)" # adds pyenv setup to environment
eval "$(pyenv virtualenv-init -)" # adds virtualenv setup to environment

# [print pyenv version]
pyenv --version

# [verify pyenv installation]

# Check that the pyenv hook is installed:
[ -n "$PYENV_SHELL" ]

# Check that the pyenv virtualenv hook is installed:
[ "$PYENV_VIRTUALENV_INIT" = "1" ]

##
## Install python 3.10.11 through pyenv
##
## References:
##  - https://github.com/pyenv/pyenv#usage

# [install python through pyenv]
pyenv install 3.10.11
pyenv global 3.10.11

# [print python version]
python --version # should output 3.10.11

# [verify python version]
[ "$(python --version)" = "Python 3.10.11" ]

##
## Install poetry
##
## References:
##  - https://python-poetry.org/docs/master/#installing-with-the-official-installer

# [install poetry]
curl -sSL https://install.python-poetry.org -o install-poetry.py
python install-poetry.py --version 1.1.13 --yes

# [setup shell for poetry]
export PATH=$PATH:$HOME/.local/bin

# [print poetry version]
poetry --version # should output 1.1.13

# [verify poetry version]
[ "$(poetry --version)" = "Poetry version 1.1.13" ]

##
## Test installing Octez python development dependencies
##
git clone --depth 1 --branch "$BRANCH" https://gitlab.com/"$REPO".git tezos

# [install octez python dev-dependencies]
cd tezos
poetry install

# [print sphinx-build versions]
poetry run sphinx-build --version # should output 4.2.0

# [verify sphinx-build version]
[ "$(poetry run sphinx-build --version)" = "sphinx-build 4.2.0" ]
