#!/bin/sh

# SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
#
# SPDX-License-Identifier: MIT

# Usage: cargo-build.sh DUNE_PROFILE OCTEZ_RUST_INCREMENTAL [cargo build options]
set -eu

profile=$1
incremental=$2
shift 2

# Use Cargo's command line to avoid changing sccache's CARGO_* environment keys.
# CARGO_INCREMENTAL=0 still overrides this setting on CI.
if [ "$profile" = "dev" ] && [ "$incremental" = "1" ]; then
  set -- "$@" --config profile.release.incremental=true
fi

exec cargo build "$@"
