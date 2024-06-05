#!/bin/sh
set -eu

script_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
repo_dir="$(dirname "$script_dir")"
cd "$repo_dir"

# shellcheck source=./scripts/docker.sh
. "$script_dir"/docker.sh

# shellcheck source=./scripts/version.sh
. "$script_dir"/version.sh

if [ "${1:-}" = "--help" ]; then
  cat << EOT
Usage: $0 [image_name [tag_suffix [tag_cache_suffix [tag_extra [targetarch [layer_targets]]]]]]

This script creates all the tezos/tezos build-deps images from the
file 'Dockerfile'. Each image corresponds to a target in that
file. The set of all targets are in the variable 'docker_images' from
'docker.sh'.

Each image is named 'image_name:image_tag'. If 'image_name' is
omitted, it defaults to 'tezos/opam-repository'. The tag 'image_tag'
will be on the form 'target[--tag_suffix][--targetarch]'. 'tag_suffix'
typically identifies the revision of the repository. If omitted,
'targetarch' defaults to 'amd64'.

Images are built with buildkit inline layer caching. When building,
caches are fetched from 'image_name:image_tag_cache'. The tag
'image_tag_cache' is on the form
'target[--tag_cache_suffix][--targetarch]'. 'tag_cache_suffix'
typically identifies an older revision of the repository.

If 'tag_extra' is given, images are aliased with
'image_name:image_tag_extra' and caches are also fetched from
that same name. The tag 'image_tag_extra' will be on the form
'target[--tag_extra][--targetarch]'. 'tag_extra' typically identifies
the sanitized branch name.

Finally, 'layer_targets', if supplied, should be a space-separated list
of targets to build. By default all valid targets are built, which are:

- $(echo "$docker_images" | sed "s/ /\n- /g")
EOT
  exit 1
fi

image_name="${1:-tezos/opam-repository}"
tag_suffix="${2:-}"
tag_cache_suffix="${3:-}"
tag_extra="${4:-}"
targetarch="${5:-amd64}"
layer_targets=${6:-"$docker_images"}

build() {
  f_image_target="$1"

  f_image_tag=$(docker_tag "$f_image_target" "$targetarch" "$tag_suffix")
  f_image_tag_cache=$(docker_tag "$f_image_target" "$targetarch" "$tag_cache_suffix")
  f_image_tag_extra=$(docker_tag "$f_image_target" "$targetarch" "$tag_extra")

  echo
  echo "### Building ${f_image_target} image ($(docker_cache_disabled_pp))"
  echo "### (cache from: $image_name:$f_image_tag_cache)"
  if [ -n "$tag_extra" ]; then
    echo "### (cache from: $image_name:$f_image_tag_extra)"
  fi
  echo

  # shellcheck disable=SC2046
  docker_build \
    -f Dockerfile \
    --target="$f_image_target" \
    --build-arg BUILDKIT_INLINE_CACHE=1 \
    --cache-from="$image_name:$f_image_tag_cache" \
    $(if [ -n "$tag_extra" ]; then echo "--cache-from $image_name:$f_image_tag_extra"; fi) \
    --build-arg BUILD_IMAGE="alpine:${alpine_version}" \
    --build-arg OCAML_VERSION="${ocaml_version}" \
    --build-arg TARGETARCH="${targetarch}" \
    -t "$image_name:$f_image_tag" \
    $(if [ -n "$tag_extra" ]; then echo "-t $image_name:$f_image_tag_extra"; fi) \
    "$repo_dir"
}

for target in $layer_targets; do
  build "$target"
done
