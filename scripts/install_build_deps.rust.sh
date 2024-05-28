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

if ! [[ "$(rustc --version | cut -d' ' -f2)" == *"$rust_version"* ]]; then
  echo "\
Wrong Rust version. This is probably because you have used 'rustup
override' in the past. Run the following command from your
favorite shell, and retry to install the dependencies:
$ rustup override unset"
  exit 1
fi
