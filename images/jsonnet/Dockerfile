# Grafazos dependencies

FROM debian:stable

SHELL ["/bin/bash", "-euo", "pipefail", "-c"]

WORKDIR /root

# common packages
# hadolint ignore=DL3008
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    ca-certificates curl file \
    build-essential \
    git \
    autoconf automake autotools-dev libtool xutils-dev clang cmake \
    sccache \
    musl-tools && \
    rm -rf /var/lib/apt/lists/*

ENV PATH=/root/go/bin:$PATH

# Install go-jsonnet
RUN apt-get update && \
    apt-get install --no-install-recommends -y golang-go=2:1.19~1 && \
    # The source commit of v0.6.0 is pinned
    # https://github.com/jsonnet-bundler/jsonnet-bundler/tree/v0.6.0
    go install -a github.com/jsonnet-bundler/jsonnet-bundler/cmd/jb@ddded59 && \
    # https://github.com/google/go-jsonnet/tree/v0.20.0 (for [jsonnet], [jsonnet-lint] and [jsonnetfmt])
    go install github.com/google/go-jsonnet/cmd/jsonnetfmt@7903819 && \
    go install github.com/google/go-jsonnet/cmd/jsonnet-lint@7903819 && \
    go install github.com/google/go-jsonnet/cmd/jsonnet@7903819 && \
    rm -rf /var/lib/apt/lists/*
