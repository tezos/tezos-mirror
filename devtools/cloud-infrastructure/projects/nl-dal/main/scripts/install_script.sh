#!/usr/bin/env bash

set -xe

echo "[install_script.sh] Pulling docker..."
docker-credential-gcr configure-docker --registries="${docker_registry_url:?}"
docker pull "${docker_registry_url}/${container_image_name:?}:latest"
# docker pull debian
echo "[install_script.sh] Done"
