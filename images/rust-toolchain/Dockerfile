# rust dependencies
#
# This image includes
# - rust dependencies
#
# This image is intended for
# - building kernels, testing kernels and building the kernel SDK in the tezos/tezos CI
# - building the EVM kernel distributed in the tezos/tezos Docker images.

FROM debian:sid

SHELL ["/bin/bash", "-euo", "pipefail", "-c"]

ENV LANG='C.UTF-8' LC_ALL='C.UTF-8' TZ='Etc/UTC'

WORKDIR /root

COPY ./scripts/install_datadog_static.sh /tmp
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
    rm -rf /var/lib/apt/lists/* && \
    /tmp/install_datadog_static.sh && \
    rm /tmp/install_datadog_static.sh

# install toolchain
RUN curl https://sh.rustup.rs --silent --show-error --fail | \
    sh -s -- --default-toolchain stable -y

ENV PATH=/root/.cargo/bin:$PATH

# install rust toolchains and compilation targets
RUN rustup update 1.66.0 1.73.0 1.76.0 1.78.0 1.80.1 1.84.1 1.86.0 nightly-2025-01-30 nightly-2023-12-15 && \
    for tc in $(rustup toolchain list | awk '{print $1}'); do \
        rustup target add --toolchain="$tc" wasm32-unknown-unknown; \
        rustup target add --toolchain="$tc" riscv64gc-unknown-none-elf; \
        rustup target add --toolchain="$tc" riscv64gc-unknown-linux-gnu; \
        rustup target add --toolchain="$tc" riscv64gc-unknown-linux-musl; \
        rustup target add --toolchain="$tc" x86_64-unknown-linux-musl; \
    done && \
    rustup component add --toolchain=nightly-2025-01-30 rustfmt miri rust-src && \
    rustup component add --toolchain=nightly-2023-12-15 rust-src llvm-tools

# Install Taplo
RUN cargo +1.86.0 install taplo-cli --version 0.9.3 --locked && \
    taplo --version

# Install cargo-audit
RUN cargo +1.86.0 install --locked cargo-audit --version 0.21.2 && \
    cargo audit --version

# install wabt: https://packages.debian.org/source/sid/wabt
# Install a version of binutils which supports RISC-V and put symlink in PATH
# hadolint ignore=DL3008
RUN apt-get update \
    && apt-get install --no-install-recommends -y wabt binutils-riscv64-unknown-elf gcc-riscv64-linux-gnu \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/* \
    && ln -s /usr/bin/riscv64-linux-gnu-gcc /usr/bin/riscv64-unknown-linux-musl-gcc

RUN ln -s /usr/bin/riscv64-unknown-elf-objdump /usr/local/bin/objdump
