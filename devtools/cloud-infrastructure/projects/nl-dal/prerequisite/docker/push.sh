#!/usr/bin/env bash

set -e

if [ $# -lt 2 ]; then
  echo "At least two arguments are required: the OS name (eg 'debian'), and the path to an existing ssh public key. Use a third argument (any string) when pushing the Docker image for the first time."
  exit 1
fi

os="${1}"
docker_image="$os-tezos"
ssh_public_key_file="${2}"
# Set this argument when pushing the Docker image for the first time.
# This will reinitialize the authorized_keys file
bootstrap="${3}"

# We ask the url of the registry to terraform.
docker_registry_url=$(terraform output --raw docker_registry_url)

gcp_docker_registry=$(terraform output --raw gcp_docker_registry)

if [ -z "$ssh_public_key_file" ]; then
  echo "Error: The script should take as second argument the path towards an existing ssh public key."
  exit 1
else
  ssh_public_key=$(cat "$ssh_public_key_file")
fi

if [ -z "$bootstrap" ]; then
  source_image=$docker_registry_url/$docker_image
else
  source_image=$os
fi

echo "$source_image"

# We must run docker build at the root of the project so that docker
# can copy the zcash params into the image

prefix=$(git rev-parse --show-toplevel)

zcash_params_path="_opam/share/zcash-params"

current_directory=$(pwd)

cd "$prefix"

echo "Authentificate with gcloud..."

# Authentication for pushing to the artifact registry from gcp
gcloud auth configure-docker "$gcp_docker_registry"

echo "Start building the docker image..."

echo "Please make sure you are in the same directory as the script when you run it."

# Build the docker image specific to the os with the ssh public key given
docker build -f "$current_directory/${os}.Dockerfile" \
  --build-arg "SOURCE_IMAGE=${source_image}" \
  --build-arg "SSH_PUBLIC_KEY=${ssh_public_key}" \
  --build-arg "ZCASH_PARAMS_PATH=${zcash_params_path}" \
  -t "$docker_image:latest" \
  "."

echo "Tag the docker image..."

# Tag the image built for the artifact registry
docker tag "$docker_image:latest" "$docker_registry_url/$docker_image:latest"

echo "Push the image to the registry $docker_registry_url"

docker push "$docker_registry_url/$docker_image:latest"

echo "Done"
