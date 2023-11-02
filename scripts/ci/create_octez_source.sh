#!/bin/sh
set -eu

### Creates a directory for octez sources

# The source will be store in $SOURCE_PATH

mkdir -v "$SOURCE_PATH"

# TO DO: Define that somewhere else than in this script

cp -r src/ "$SOURCE_PATH"
cp -r tezt/ "$SOURCE_PATH"
cp -r opam/ "$SOURCE_PATH"
cp -r scripts/ "$SOURCE_PATH"
cp -r script-inputs/ "$SOURCE_PATH"
cp -r vendors/ "$SOURCE_PATH"
cp ./Makefile "$SOURCE_PATH"
cp dune "$SOURCE_PATH"
cp dune-project "$SOURCE_PATH"
cp dune-workspace "$SOURCE_PATH"
cp LICENSE "$SOURCE_PATH"
cp .ocamlformat "$SOURCE_PATH"
cp README.md "$SOURCE_PATH"
