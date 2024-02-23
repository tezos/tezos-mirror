#!/bin/sh
set -eu

### Prepare etherlink release by building wasm and preimages with docker

current_dir=$(cd "$(dirname "${0}")" && pwd)

. scripts/ci/docker.env

# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

# produce evm wasms and preimages
etherlink/scripts/build-wasm.sh etherlink/config/dev.yaml kernels

# compress the directory
tar -czf kernels.tar.gz kernels

# expose job id
echo "$CI_JOB_ID" > kernels_job_id
