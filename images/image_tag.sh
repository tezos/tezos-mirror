#!/bin/sh

set -eu

images_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$images_dir")"

usage() {
  cat << EOT
Usage: $0 images/[image]

Compute tag for [image] based on its input committed or staged state.

Note that the tag of an image does not change if a change to to its
inputs is not commited or staged in git.

Example:
  $0 images/rust-toolchain/

EOT
  exit 1
}

if [ "${1:-}" = "--help" ]; then
  usage
fi

# Paths in the 'inputs'  file are relative to the root, so go there first.
cd "$src_dir"

image_directory="${1:-}"
if [ ! -f "${image_directory}/inputs" ]; then
  echo "Argument must be a sub-folder in 'images/' containing an 'inputs' file."
  exit 1
fi

# Sanity check that each line in the image's inputs corresponds to an
# existing file or directory:
while read -r input; do
  if [ ! -e "$input" ]; then
    echo "${image_directory}/inputs: ${input} does not exist" >&2
    exit 1
  fi
done < "${image_directory}/inputs"

# the tag is the hash of this image's input which is the set of paths
# defined in images/${image_directory}/inputs. Requires wordsplitting
# on the inputs.
# shellcheck disable=SC2046
git ls-files --stage -- $(cat "${image_directory}/inputs") | git hash-object --stdin
