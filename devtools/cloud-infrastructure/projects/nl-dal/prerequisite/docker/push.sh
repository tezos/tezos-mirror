#!/usr/bin/env bash

set -e

# Ensure we are executing this script in a terraform workspace

if [ -z "$TF_WORKSPACE" ]; then
  echo "The environement variable TF_WORKSPACE is not defined"
  exit 1
else
  echo "This script will be run for the workspace=$TF_WORKSPACE"
  workspace=$TF_WORKSPACE
fi

# Check whether an SSH public key for this workspace exists

default_ssh_basename="${workspace}-tf"
default_ssh_filepath="$HOME/.ssh/${default_ssh_basename}.pub"

if [ -f "${default_ssh_filepath}" ]; then
  echo "The public SSH key ${default_ssh_filepath} was found"
  ssh_public_key=$(cat "${default_ssh_filepath}")
else
  echo "Please create an ssh key with the name ${default_ssh_basename} in the default ssh directory"
  exit 1
fi

echo "Ask to terraform the registry URL"

# We ask the url of the registry to terraform.

terraform refresh

docker_registry_url=$(terraform output --raw docker_registry_url)

gcp_docker_registry=$(terraform output --raw gcp_docker_registry)

echo "Registry URL found: ${docker_registry_url}"

echo "Authentificate with gcloud..."

# Authentication for pushing to the artifact registry from gcp
gcloud auth configure-docker "$gcp_docker_registry"

echo "Start building the docker image..."

echo "Please make sure you are in the same directory as the script when you run it."

# We must run docker build at the root of the project so that docker
# can copy the zcash params into the image

prefix=$(git rev-parse --show-toplevel)

zcash_params_path="_opam/share/zcash-params"

current_directory=$(pwd)

cd "${prefix}"

pwd

# Build the docker image specific to the os with the ssh public key given
docker build -f "$current_directory/${workspace}.Dockerfile" \
  --build-arg "SSH_PUBLIC_KEY=${ssh_public_key}" \
  --build-arg "ZCASH_PARAMS_PATH=${zcash_params_path}" \
  -t "${workspace}:latest" \
  "."

echo "Tag the docker image..."

# Tag the image built for the artifact registry
docker tag "${workspace}:latest" "$docker_registry_url/${workspace}:latest"

echo "Push the image to the registry $docker_registry_url"

docker push "$docker_registry_url/${workspace}:latest"

echo "Done"
