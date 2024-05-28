#!/bin/sh
set -eu

# To run after the final Docker image was built on top of all the
# other ones to display various software versions

current_dir=$(cd "$(dirname "${0}")" && pwd)
images_dir="$(dirname "${current_dir}")"

# shellcheck source=./images/scripts/check_version.inc.sh
. "${images_dir}/scripts/check_version.inc.sh"

if [ $# -gt 0 ]; then
  image_base="$1"
  shift
else
  image_base="registry.gitlab.com/tezos/tezos/client-lib-dependencies"
fi
if [ $# -gt 0 ]; then
  image_tag="$1"
  shift
else
  image_tag="latest"
fi
image_name="${image_base}:${image_tag}"

run="docker run --rm ${image_name}"

echo "###"
echo "### Version control for ${image_name}"
echo "###"

# shellcheck source=./images/client-libs-dependencies/version.sh
. "${current_dir}/version.sh"

echo "### Distro info"
eval "${run} cat /etc/os-release"

echo "### Important packages version"

current_ksc_version=$(${run} kaitai-struct-compiler --version | awk '{print $2}')
check_version kaitai-struct-compiler "${current_ksc_version}" "${kaitai_struct_compiler_version}"

die_if_error
