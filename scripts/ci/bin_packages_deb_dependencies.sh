#!/bin/sh

export DEBIAN_FRONTEND=noninteractive

apt update
apt-get install -y rsync git m4 build-essential patch unzip wget \
  opam jq bc autoconf cmake libev-dev libffi-dev libgmp-dev \
  libhidapi-dev pkg-config zlib1g-dev libprotobuf-dev \
  protobuf-compiler libsqlite3-dev
