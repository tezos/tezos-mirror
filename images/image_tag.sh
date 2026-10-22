#!/bin/sh

set -eu

# This script and the images/ci/inputs file it reads are what is left of the
# input-hash machinery. The images it tags -- the dynamic ci/* images -- are
# gone, so its two remaining callers, ci/makefile and
# devtools/testnet_experiment_tools/docker/create_docker_image.sh, compute a tag
# for an image that no longer exists. Once both name the static alpine-* images
# instead, this script and images/ci/inputs can be deleted. See #8383.
#
# The note lives here rather than in images/ci/inputs because every line of that
# file has to be an existing path: see the sanity check below.

images_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
src_dir="$(dirname "$images_dir")"

usage() {
  cat << EOT
Usage: $0 images/[image]

Compute tag for [image] based on its input committed or staged state.

Note that the tag of an image on changes if changes to its input are
staged or committed in git.

Example:
  $0 images/ci/

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
  echo "Argument must be a path in 'images/' containing an 'inputs' file, i.e. one of:"
  find "${images_dir}" -mindepth 2 -maxdepth 2 -name inputs -exec dirname \{\} \; | while read -r image_dir; do
    echo "${image_dir#"${src_dir}"/}"
  done
  exit 1
fi

# Sanity check that each line in the image's inputs corresponds to an
# existing file or directory and its git status:
while read -r input; do
  if [ ! -e "$input" ]; then
    echo "${image_directory}/inputs: ${input} does not exist" >&2
    exit 1
  fi
  if ! git diff-files --quiet -- "$input"; then
    echo "Warning: ${image_directory}/inputs: ${input} has unstaged changes which will be ignored." >&2
  fi
done < "${image_directory}/inputs"

# the tag is the hash of this image's input which is the set of paths
# defined in images/${image_directory}/inputs. Requires wordsplitting
# on the inputs.
# shellcheck disable=SC2046
git ls-files --stage -- $(cat "${image_directory}/inputs") | git hash-object --stdin
