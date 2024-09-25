#!/usr/bin/env bash

set -e

script_dir="$(cd "$(dirname "$0")" && pwd -P)"

#shellcheck source=scripts/version.sh
. "$script_dir"/version.sh

# This script verifies a Rust system with the correct version is setup on the
# machine. It assumes that cargo is installed in the system, use your package
# manager or https://rustup.rs by specifying the env var RUST_VERSION the user
# can decide to use a different version of rust (recommended_rust_version is a
# variable declared in scripts/version.sh)
rust_version=${RUST_VERSION:-$recommended_rust_version}

if [ "$recommended_rust_version" != "$rust_version" ]; then
  echo "\
WARNING: you selected a different version of rust. Tezos is tested only
with Rust $recommended_rust_version. Do this at your own peril."
  sleep 3
fi

if [ ! -x "$(command -v rustup)" ] &&
  [[ ! -x "$(command -v rustc)" || ! -x "$(command -v cargo)" ]]; then
  echo "The Rust compiler is not installed. Please install Rust $recommended_rust_version."
  echo "See instructions at: https://tezos.gitlab.io/introduction/howtoget.html#setup-rust"
  exit 1
fi

current_rust_version="$(rustc --version | cut -d' ' -f2)"

if ! [[ "$current_rust_version" == *"$rust_version"* ]]; then
  # Note: since the addition of the rust-toolchain file,
  # we do not recommend to use 'rustup override set $recommended_rust_version'.
  echo "\
Wrong Rust version ($current_rust_version instead of $rust_version).

If you are using rustup, you can install the recommended version with:

    rustup install $recommended_rust_version

'rustup show' should show that the active toolchain is overridden by the
'rust-toolchain' file from this repository.
If you have used 'rustup override' in the past you may need to run:

    rustup override unset

See more information here:
http://tezos.gitlab.io/introduction/howtoget.html#install-rust

Alternatively, you can try to continue anyway by setting
the RUST_VERSION environment variable:

    RUST_VERSION=$current_rust_version make build-deps
"
  exit 1
fi
