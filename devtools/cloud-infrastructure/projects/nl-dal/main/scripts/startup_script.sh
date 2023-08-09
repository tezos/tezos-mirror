#!/bin/bash

docker run \
  -p 30000-30999:30000-30999 \
  --name tezos europe-west1-docker.pkg.dev/nl-dal/docker-registry/debian-tezos@sha256:latest \
  bash -c "$(cat /tmp/start.sh)"
