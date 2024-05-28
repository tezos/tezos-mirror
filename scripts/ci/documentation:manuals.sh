#!/bin/sh

set -ex

./scripts/remove-old-protocols.sh .trash
make all
./scripts/restore-old-protocols.sh .trash
make -C docs -j octez-gen
