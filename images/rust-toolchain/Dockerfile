ARG IMAGE=invalid
# the image with proper version is set as ARG
#hadolint ignore=DL3006
FROM ${IMAGE} AS rust-toolchain

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
RUN cargo +1.88.0 install --locked cargo-audit --version 0.22.0 && \
    cargo audit --version

# Install cargo-expand, used to test derive macros in sdk/rust/encoding-derive/
RUN cargo +1.88.0 install cargo-expand
