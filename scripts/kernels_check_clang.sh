#!/usr/bin/env bash

# Copyright 2023 Trilitech <contact@trili.tech>
# SPDX-License-Identifier: MIT

if [ -z "$CC_wasm_unknown_unkown" ]; then
  CLANG=$(which clang 2> /dev/null)

  CLANG_HAS_WASM=$([ -e "$CLANG" ] && $CLANG -print-targets | grep 'wasm32')

  if [ -z "$CLANG_HAS_WASM" ]; then
    echo "#############################################################################"
    echo "default clang '$CLANG' missing toolchain 'wasm32-unknown-unknown'"
    echo "recommend setting 'CC_wasm_unknown_unknown' to a more recent install of clang"
    echo ""
    echo "'\$CC_wasm_unknown_unknown -print-targets' should contain 'wasm32'"
    echo "#############################################################################"
    exit 1
  fi
fi
