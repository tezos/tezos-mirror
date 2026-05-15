# Rust SDK bindings dependencies
#
# This image includes
# - rust dependencies
# - python dependencies
#
# This image is intended for
# - building Rust SDK bindings and testing bindings in the tezos/tezos CI
#
# This image derives from the base debian image which already provides:
# ca-certificates, curl, jq, gpg, kiss-fetch, datadog and gcloud.

ARG IMAGE=debian:trixie
#hadolint ignore=DL3006
FROM ${IMAGE}

SHELL ["/bin/bash", "-euo", "pipefail", "-c"]

ENV LANG='C.UTF-8' LC_ALL='C.UTF-8'

WORKDIR /root

# install sccache (not in base image)
COPY scripts/install_sccache_static.sh /tmp/
#hadolint ignore=DL3008,DL3015
RUN /tmp/install_sccache_static.sh && \
    rm /tmp/install_sccache_static.sh

# SDK bindings dependencies
# hadolint ignore=DL3008
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    # Build requirements
    build-essential \
    git \
    pkg-config \
    tzdata \
    # Python requirements
    python3 python3-venv \
    pipx \
    # Kotlin requirements
    openjdk-21-jdk \
    # Swift compiler (available as apt package on trixie)
    swiftlang \
    # Swift build dependencies
    libcurl4-openssl-dev \
    libedit-dev \
    libicu-dev \
    libncurses-dev \
    libpython3-dev \
    libsqlite3-dev \
    libxml2-dev \
    uuid-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# install Rust toolchain
ARG RUST_VERSION=1.83.0
RUN curl https://sh.rustup.rs --silent --show-error --fail | \
    sh -s -- --default-toolchain ${RUST_VERSION} -y
ENV PATH=/root/.cargo/bin:$PATH

# setup python venv and install pytest
RUN python3 -m venv .venv && \
    ./.venv/bin/pip install pytest

# install maturin
ARG MATURIN_VERSION=1.5.1
RUN pipx ensurepath && pipx install maturin==${MATURIN_VERSION}

ENV PATH="/root/.local/bin/:$PATH"
