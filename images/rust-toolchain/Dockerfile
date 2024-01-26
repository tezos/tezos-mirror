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

# common packages
# hadolint ignore=DL3008
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
    ca-certificates curl file \
    build-essential \
    git \
    autoconf automake autotools-dev libtool xutils-dev clang \
    sccache && \
    rm -rf /var/lib/apt/lists/*

ENV SSL_VERSION=1.0.2u

# hadolint ignore=DL3003
RUN curl https://www.openssl.org/source/openssl-$SSL_VERSION.tar.gz -O && \
    tar -xzf openssl-$SSL_VERSION.tar.gz && \
    cd openssl-$SSL_VERSION && ./config && make depend && make install && \
    cd .. && rm -rf openssl-$SSL_VERSION*

ENV OPENSSL_LIB_DIR=/usr/local/ssl/lib \
    OPENSSL_INCLUDE_DIR=/usr/local/ssl/include \
    OPENSSL_STATIC=1

# install toolchain
RUN curl https://sh.rustup.rs --silent --show-error --fail | \
    sh -s -- --default-toolchain stable -y

ENV PATH=/root/.cargo/bin:$PATH

# install rust toolchains and compilation targets
RUN rustup update 1.66.0 1.71.1 1.73.0 && \
    for tc in $(rustup toolchain list | awk '{print $1}'); do \
        rustup target add --toolchain="$tc" \
            wasm32-unknown-unknown \
            riscv64gc-unknown-none-elf \
            riscv64gc-unknown-linux-gnu \
            x86_64-unknown-linux-musl; \
    done

# Install Rust 1.73.0 standard library for riscv64gc-unknown-hermit
RUN curl -L "https://github.com/hermit-os/rust-std-hermit/releases/download/1.73.0/rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz" -o rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz \
    && (echo 65980ab1110081a6b7edd70b45bebe63a50e34a6e1555d6fc000c35360907547 rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz | sha256sum -c) \
    && tar xf rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz \
    && rust-std-1.73.0-riscv64gc-unknown-hermit/install.sh \
    && rm -r rust-std-1.73.0-riscv64gc-unknown-hermit rust-std-1.73.0-riscv64gc-unknown-hermit.tar.gz

# install wabt: https://packages.debian.org/source/sid/wabt
# hadolint ignore=DL3008
RUN apt-get update \
    && apt-get install --no-install-recommends -y wabt \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*
