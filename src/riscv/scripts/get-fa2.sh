#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
#
# SPDX-License-Identifier: MIT

# Build and install the fa2 example contract from jstz

set -e

CURR=$(pwd)

JSTZ_COMMIT=20ba89084d233c9f180d7bb86bd33af2ef25dc7f
JSTZ_DIR=$(mktemp -d)

JSTZ_WORKSPACE=$(git rev-parse --show-toplevel)/src/riscv/jstz

git clone https://github.com/trilitech/jstz.git "${JSTZ_DIR}" --depth 1
cd "${JSTZ_DIR}"

git fetch && git checkout $JSTZ_COMMIT
cd examples/fa2

npm install && npm run build

cp dist/index.js "${JSTZ_WORKSPACE}"/fa2.js

cd "${CURR}"
