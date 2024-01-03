#!/bin/sh

set -e

if [ -z "$recommended_node_version" ]; then
  script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
  if [ -f "${script_dir}"/version.sh ]; then
    #shellcheck source=scripts/version.sh
    . "$script_dir"/version.sh
  else
    echo "\$recommended_node_version is undefined, please source scripts/version.sh"
    exit 1
  fi
fi

# nvm is a runtime dependency now.
export NODE_VERSION="${NODE_VERSION:-${recommended_node_version}}"

export NVM_NODEJS_ORG_MIRROR=https://unofficial-builds.nodejs.org/download/release
export NVM_DIR="$HOME/.nvm"
#shellcheck disable=SC1091
. "$HOME/.nvm/nvm.sh"

# override nvm_get_arch, as suggested in https://github.com/nvm-sh/nvm/issues/1102
nvm_get_arch() {
  HOST_ARCH=''
  NVM_OS=''

  NVM_OS="$(nvm_get_os)"
  # If the OS is SunOS, first try to use pkgsrc to guess
  # the most appropriate arch. If it's not available, use
  # isainfo to get the instruction set supported by the
  # kernel.
  if [ "_${NVM_OS}" = "_sunos" ]; then
    if HOST_ARCH=$(pkg_info -Q MACHINE_ARCH pkg_install); then
      HOST_ARCH=$(nvm_echo "${HOST_ARCH}" | command tail -1)
    else
      HOST_ARCH=$(isainfo -n)
    fi
  elif [ "_${NVM_OS}" = "_aix" ]; then
    HOST_ARCH=ppc64
  else
    HOST_ARCH="$(command uname -m)"
  fi

  NVM_ARCH=''
  case "${HOST_ARCH}" in
  x86_64 | amd64) NVM_ARCH="x64" ;;
  i*86) NVM_ARCH="x86" ;;
  aarch64) NVM_ARCH="arm64" ;;
  *) NVM_ARCH="${HOST_ARCH}" ;;
  esac
  # The next three lines are used to support alpine
  if (ldd "$(which echo)" | nvm_grep -q musl); then
    NVM_ARCH="${NVM_ARCH}-musl"
  fi
  nvm_echo "${NVM_ARCH}"
}

nvm install "$NODE_VERSION"
nvm use --delete-prefix "$NODE_VERSION"

echo "Check versions"
node --version

# [npm ci] is like [npm install] but will fail if [package.json] and
# [package-lock.json] disagree. It also removes any pre-existing [node_modules] install.
if [ -z "${CI_PROJECT_DIR}" ]; then
  npm install
else
  npm ci
fi
