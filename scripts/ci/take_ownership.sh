#!/bin/sh

# FIXME: https://gitlab.com/tezos/tezos/-/issues/2865
# Used to retake ownership of the checkout, to work around an issue in
# GitLab CI runner.

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$(dirname "$script_dir")")"

sudo chown -R "$(id -u)":"$(id -g)" "$src_dir"
