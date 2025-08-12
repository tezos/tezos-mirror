FROM debian:sid AS rust-base

SHELL ["/bin/bash", "-euo", "pipefail", "-c"]

ENV LANG='C.UTF-8' LC_ALL='C.UTF-8' TZ='Etc/UTC'

WORKDIR /root

# Setup proxy
ARG APT_PROXY
ENV APT_PROXY=${APT_PROXY:-false}
RUN echo "Acquire::http::Proxy::deb.debian.org \"$APT_PROXY\";" > /etc/apt/apt.conf.d/01proxy

# install datadog static binary
COPY scripts/install_datadog_static.sh /tmp
#hadolint ignore=DL3008,DL3015
RUN apt-get update && \
  apt-get -y install curl && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/* && \
  /tmp/install_datadog_static.sh

# common packages to build rust
# hadolint ignore=DL3008,DL3015
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    ca-certificates file \
    build-essential \
    git \
    gcc \
    tzdata \
    pkg-config autoconf automake autotools-dev libtool xutils-dev clang cmake \
    sccache \
    musl-tools \
    wabt binutils-gold binutils-riscv64-unknown-elf gcc-riscv64-linux-gnu && \
    ln -s /usr/bin/riscv64-linux-gnu-gcc /usr/bin/riscv64-unknown-linux-musl-gcc && \
    ln -s /usr/bin/riscv64-unknown-elf-objdump /usr/local/bin/objdump && \
    rm -rf /var/lib/apt/lists/*

# install toolchain
RUN curl https://sh.rustup.rs --silent --show-error --fail | \
    sh -s -- --default-toolchain stable -y

ENV PATH=/root/.cargo/bin:$PATH

# install rust toolchains and compilation targets
RUN rustup update 1.66.0 1.73.0 1.76.0 1.78.0 1.80.1 1.84.1 1.86.0 1.88.0 nightly-2025-01-30 nightly-2023-12-15 && \
    for tc in $(rustup toolchain list | awk '{print $1}'); do \
        rustup target add --toolchain="$tc" wasm32-unknown-unknown; \
        rustup target add --toolchain="$tc" riscv64gc-unknown-none-elf; \
        rustup target add --toolchain="$tc" riscv64gc-unknown-linux-gnu; \
        rustup target add --toolchain="$tc" riscv64gc-unknown-linux-musl; \
        rustup target add --toolchain="$tc" x86_64-unknown-linux-musl; \
    done && \
    rustup component add --toolchain=nightly-2025-01-30 rustfmt miri rust-src && \
    rustup component add --toolchain=nightly-2023-12-15 rust-src llvm-tools

FROM rust-base AS rust-toolchain

# rust dependencies
#
# This image includes
# - rust dependencies
#
# This image is intended for
# - building kernels, testing kernels and building the kernel SDK in the tezos/tezos CI
# - building the EVM kernel distributed in the tezos/tezos Docker images.

# Install Taplo
RUN cargo +1.88.0 install taplo-cli --version 0.9.3 --locked && \
    taplo --version

# Install cargo-audit
RUN cargo +1.88.0 install --locked cargo-audit --version 0.21.2 && \
    cargo audit --version

# Install cargo-expand, used to test derive macros in sdk/rust/encoding-derive/
RUN cargo +1.88.0 install cargo-expand
