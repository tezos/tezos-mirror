#!/bin/sh

set -e

# Static executables are located in octez-binaries/x86_64,
# but the 'octez-gen' make target expects them at the root of the project.
# This script thus moves those executables.

# We use a for loop instead of mv to be able to print the list of executables
# that we found.
for file in octez-binaries/x86_64/octez-*; do
  echo "mv $file $PWD"
  mv "$file" .
done

echo "make -C docs -j octez-gen"
make -C docs -j octez-gen
