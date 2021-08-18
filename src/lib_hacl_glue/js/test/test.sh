#!/bin/sh

# nvm is a runtime dependency now.
NODE_VERSION=14.12.0

export NVM_NODEJS_ORG_MIRROR=https://unofficial-builds.nodejs.org/download/release
export NVM_DIR="$HOME/.nvm"
#shellcheck disable=SC1090
. "$HOME/.nvm/nvm.sh"

nvm_get_arch() { nvm_echo "x64-musl"; }

nvm install $NODE_VERSION
nvm use --delete-prefix $NODE_VERSION
npm install --global yarn

echo "Test executed using"
node --version
yarn --version

yarn
yarn add @nomadic-labs/hacl-wasm

# here we run directly a test in js compiled with jsoo
# instead of a mocha test
node test.bc.js
