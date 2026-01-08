#!/bin/sh
set -eu

if [ "${TRACE:-}" ]; then set -x; fi

images_dir="$(cd "$(dirname "$0")" && echo "$(pwd -P)/")"
script_dir="$(dirname "$images_dir")/scripts"
repo_dir="$(dirname "$images_dir")"
cd "$repo_dir"

# shellcheck source=./images/ci.inc.sh
. "$images_dir"/ci.inc.sh

# shellcheck source=./scripts/version.sh
. "$script_dir"/version.sh

usage() {
  cat << EOT
Usage: $0 [-h|--help]
  [--image-base <IMAGE_BASE>]
  [--tag-suffix <TAG_SUFFIX>]
  [--tag-cache-suffix <TAG_CACHE_SUFFIX>]
  [--tag-extra <TAG_EXTRA>]
  [--targetarch <TARGETARCH>]
  [--layer-targets <LAYER_TARGETS>]
  [-- docker build args]

Creates all the tezos/tezos CI images from
'images/ci/Dockerfile'. Each image corresponds to a
LAYER_TARGET in that file. The set of all targets are in the variable
'docker_images' of 'ci.inc.sh'. For more info on
different targets, see 'images/ci/README.md'.

Each image is named IMAGE_BASE/LAYER_TARGET:IMAGE_TAG. If IMAGE_BASE
is omitted, it defaults to the value of
  $ci_image_name
from 'scripts/version.sh'. The tag IMAGE_TAG will be on the form
TARGETARCH[--TAG_SUFFIX]. If omitted, TARGETARCH defaults to
'amd64'. The only other valid value is 'arm64'. TAG_SUFFIX typically
identifies an input hash. See 'README.md' and 'image_tag.sh' in this
folder for more info on input hashes.

Images use BuildKit inline layer caching. If TAG_CACHE_SUFFIX is
supplied, then caches are fetched from
IMAGE_BASE/LAYER_TARGET:IMAGE_TAG_CACHE, where IMAGE_TAG_CACHE is on
the form TARGETARCH[--TAG_CACHE_SUFFIX]. TAG_CACHE_SUFFIX is typically
used to fetch caches from builds on the default branch.

If TAG_EXTRA is given, built images are also named
image_base/LAYER_TARGET:IMAGE_TAG_EXTRA and caches are also fetched
from that same name. The tag IMAGE_TAG_EXTRA will be on the form
TARGETARCH[--TAG_EXTRA]. TAG_EXTRA typically identifies a sanitized
branch name.

Finally, LAYER_TARGETS, if supplied, should be a space-separated list
of targets to build. By default all valid targets are built, which
are:

- $(echo "$valid_layer_targets" | sed "s/ /\n- /g")

EXAMPLES

With no argument, builds targets for amd64 tagged
  ${ci_image_name}/TARGET:amd64
and without fetching any remote caches:

  $ ./images/create_ci_images.sh

We can parameterize the script to fetch caches from a CI build of the
CI images in a given branch, e.g. 'master'. First, we
omit '--image-base', which will default to the same name used by the
tezos/tezos CI. Then, the key is to set the TAG_CACHE_SUFFIX argument
to the slugified version of the branch of interest -- here
'master':

  $ ./images/create_ci_images.sh \\
        --tag-cache-suffix master \\
        --tag-extra my_build

Assuming the result of ./images/image_tag.sh, used to set the default
value of TAG_SUFFIX is 'deadbeef', this will create the following
images:

 - runtime   :amd64--deadbeef, :amd64--my_build
 - prebuild  :amd64--deadbeef, :amd64--my_build
 - ...

under '$ci_image_name',
with caches fetched from

 - runtime   :amd64--my_build, :amd64--master
 - prebuild  :amd64--my_build, :amd64--master
 - ...

EOT
  exit 1
}

# Default values for options
image_base="$ci_image_name"
image_base_protected="$ci_image_name_protected"
tag_suffix="$(./images/image_tag.sh images/ci)"
tag_cache_suffix=""
tag_extra=""
targetarch="amd64"
layer_targets="$valid_layer_targets"

options=$(getopt -o h \
  -l help,image-base:,image-base-protected:,tag-suffix:,tag-cache-suffix:,tag-extra:,targetarch:,layer-targets: -- "$@")
eval set - "$options"

# Parse options
while true; do
  case "$1" in
  --image-base)
    shift
    image_base="$1"
    ;;
  --image-base-protected)
    shift
    image_base_protected="$1"
    ;;
  --tag-suffix)
    shift
    tag_suffix="$1"
    ;;
  --tag-cache-suffix)
    shift
    tag_cache_suffix="$1"
    ;;
  --tag-extra)
    shift
    tag_extra="$1"
    ;;
  --targetarch)
    shift
    targetarch="$1"
    case "$targetarch" in
    arm64 | amd64) ;;
    *)
      echo "Invalid architecture '$targetarch'. Should be either 'amd64' or 'arm64'. See --help."
      exit 1
      ;;
    esac

    ;;
  --layer-targets)
    shift
    layer_targets="$1"
    for target in $layer_targets; do
      if ! echo "$valid_layer_targets" | grep -q "\(^\|\s\+\)${target}\($\|\s\+\)"; then
        echo "Invalid layer target '$target'. Should be one of: $valid_layer_targets"
        exit 1
      fi
    done
    ;;
  -h | --help)
    usage
    ;;
  --)
    # This artificial option is added by 'getopt' and is required to recognize
    # the end arguments.
    shift
    break
    ;;
  :) # If expected argument omitted:
    usage
    ;;
  *) # If unknown (any other) option:
    usage
    ;;
  esac
  shift
done

build() {
  f_LAYER_TARGET="$1"
  shift

  f_image_tag=$(docker_tag "$targetarch" "$tag_suffix")
  f_image_tag_extra=$(docker_tag "$targetarch" "$tag_extra")
  f_image_tag_protected=$(docker_tag "$targetarch" "$tag_cache_suffix")

  # Name to this
  f_image_name="${image_base}/${f_LAYER_TARGET}:${f_image_tag}"
  # Fetch caches from $tag-cache-suffix that by default is $CI_DEFAULT_BRANCH
  f_image_name_protected="${image_base_protected}/${f_LAYER_TARGET}:$f_image_tag_protected"
  # Optionally, also name to this and caches from this name
  f_image_name_extra="${image_base}/${f_LAYER_TARGET}:${f_image_tag_extra}"

  echo
  echo "### Building ${f_image_name} image"
  echo "### (cache from ${f_image_name_protected})"
  if [ -n "$tag_extra" ]; then
    echo "### (cache from: ${f_image_name_extra})"
  fi
  echo

  # shellcheck disable=SC2046
  cd "${images_dir}/ci" &&
    tar -cvh . |
    docker build --network host \
      -f "Dockerfile.$f_LAYER_TARGET" \
      --build-arg BUILDKIT_INLINE_CACHE=1 \
      $(if [ -n "$tag_extra" ]; then echo "--cache-from=$f_image_name_extra"; fi) \
      --cache-from="$f_image_name_protected" \
      --build-arg BUILD_IMAGE="alpine:${alpine_version}" \
      --build-arg OCAML_VERSION="${ocaml_version}" \
      --build-arg OPAM_VERSION="${opam_version}" \
      --build-arg TARGETARCH="${targetarch}" \
      --build-arg NPM_REGISTRY_DOMAIN="${NPM_REGISTRY_DOMAIN:-}" \
      --build-arg NPM_REGISTRY="${NPM_REGISTRY:-}" \
      --build-arg GCLOUD_VERSION="${GCLOUD_VERSION}" \
      -t "$f_image_name" \
      $(if [ -n "$tag_extra" ]; then echo "-t $f_image_name_extra"; fi) \
      "$@" \
      "-"

  # add a local tag ( not pushed )
  docker image tag "$f_image_name" "$f_LAYER_TARGET"
}

for target in $layer_targets; do
  build "$target" "$@"
done
