# Rust SDK bindings dependencies
#
# This image includes
# - rust dependencies
# - python dependencies
#
# This image is intended for
# - building Rust SDK bindings and testing bindings in the tezos/tezos CI

FROM debian:bookworm

SHELL ["/bin/bash", "-euo", "pipefail", "-c"]

ENV LANG='C.UTF-8' LC_ALL='C.UTF-8' TZ='Etc/UTC'

WORKDIR /root

# common packages
# hadolint ignore=DL3008
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    # Rust requirements
    ca-certificates curl \
    build-essential \
    sccache \
    # Python requirements
    python3 python3-venv && \
    rm -rf /var/lib/apt/lists/*

# install toolchain
ARG RUST_VERSION=1.83.0
RUN curl https://sh.rustup.rs --silent --show-error --fail | \
    sh -s -- --default-toolchain ${RUST_VERSION} -y
ENV PATH=/root/.cargo/bin:$PATH

# setup python venv
RUN python3 -m venv .venv

# install pytest
RUN ./.venv/bin/pip install pytest
