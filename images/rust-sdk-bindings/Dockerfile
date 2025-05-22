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

COPY ./scripts/install_datadog_static.sh /tmp/

# common packages
# hadolint ignore=DL3008
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    # Rust requirements
    ca-certificates curl \
    build-essential \
    sccache \
    # Python requirements
    python3 python3-venv \
    # Kotlin requirements
    openjdk-17-jdk \
    # Swift dependencies
    binutils-gold \
    gcc \
    git \
    libcurl4-openssl-dev \
    libedit-dev \
    libicu-dev \
    libncurses-dev \
    libpython3-dev \
    libsqlite3-dev \
    libxml2-dev \
    pkg-config \
    tzdata \
    uuid-dev && \
    rm -rf /var/lib/apt/lists/* && \
    /tmp/install_datadog_static.sh && \
    rm /tmp/install_datadog_static.sh

# install toolchain
ARG RUST_VERSION=1.83.0
RUN curl https://sh.rustup.rs --silent --show-error --fail | \
    sh -s -- --default-toolchain ${RUST_VERSION} -y
ENV PATH=/root/.cargo/bin:$PATH

# setup python venv
# install pytest
RUN python3 -m venv .venv && \
    ./.venv/bin/pip install pytest

# install swift
ARG SWIFT_VERSION=6.0.3
ARG SWIFT_PLATFORM=debian12
ARG SWIFT_PACKAGE=swift-${SWIFT_VERSION}-RELEASE-${SWIFT_PLATFORM}
ARG SWIFT_URL=https://download.swift.org/swift-${SWIFT_VERSION}-release/${SWIFT_PLATFORM}/swift-${SWIFT_VERSION}-RELEASE/${SWIFT_PACKAGE}.tar.gz

COPY rust-sdk-bindings/${SWIFT_PACKAGE}.tar.gz.sha256 .
RUN curl -fsSL $SWIFT_URL -o ${SWIFT_PACKAGE}.tar.gz && \
    sha256sum --check ${SWIFT_PACKAGE}.tar.gz.sha256 && \
    tar xzf ${SWIFT_PACKAGE}.tar.gz && \
    rm ${SWIFT_PACKAGE}.tar.gz

ENV PATH="/root/${SWIFT_PACKAGE}/usr/bin:$PATH"
ENV LD_LIBRARY_PATH="/root/${SWIFT_PACKAGE}/usr/lib:$LD_LIBRARY_PATH"
