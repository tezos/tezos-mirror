#!/bin/sh

set -ex

./scripts/slim-mode.sh on
make all
./scripts/slim-mode.sh off
make -C docs -j octez-gen
