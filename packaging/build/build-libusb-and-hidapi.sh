#! /bin/sh

# SPDX-FileCopyrightText: 2020 TQ Tezos <https://tqtezos.com/>
#
# SPDX-License-Identifier: MPL-2.0

# This script builds and installs libusb and hidapi libraries. It's required since alpine
# doesn't provide static versions for these libraries, thus we have to compile it ourselves.
set -eu

libusb_commit="e782eeb2514266f6738e242cdcb18e3ae1ed06fa"
hidapi_commit="7da5cc91fc0d2dbe4df4f08cd31f6ca1a262418f"
git clone --single-branch --branch v1.0.23 https://github.com/libusb/libusb.git --depth 1
cd libusb
if [ "$(git rev-parse HEAD)" != "$libusb_commit" ]; then
    echo "Unexpected libusb sources"
    exit 1
fi
autoreconf -fvi && ./configure && make && sudo make install

git clone --single-branch --branch hidapi-0.9.0 https://github.com/libusb/hidapi.git --depth 1
cd hidapi
if [ "$(git rev-parse HEAD)" != "$hidapi_commit" ]; then
    echo "Unexpected hidapi sources"
    exit 1
fi
autoreconf -fvi && ./bootstrap && ./configure && make && sudo make install

rm -rf libusb hidapi
