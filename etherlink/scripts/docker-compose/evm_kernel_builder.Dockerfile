ARG RUST_IMAGE=registry.gitlab.com/tezos/tezos/rust-toolchain
ARG RUST_TAG=master
ARG BASE_IMAGE=debian:sid-slim

FROM ${RUST_IMAGE}:${RUST_TAG} AS kernel_build
RUN curl http://http.us.debian.org/debian/pool/main/p/prelink/execstack_0.0.20131005-1+b10_amd64.deb -o execstack.deb
RUN apt update && apt install -y ./execstack.deb
# Needed to clear execstack for rust 1.66 on mac os
RUN execstack -c $(find ~/.rustup/ -name libLLVM-*-rust-*-stable.so)
ARG EVM_CONFIG=etherlink/config/dev.yaml
ARG CI_COMMIT_SHA
WORKDIR /build
COPY kernels.mk etherlink.mk /build/
COPY src/kernel_sdk /build/src/kernel_sdk
COPY etherlink /build/etherlink
RUN make -f etherlink.mk build-deps
RUN make -f etherlink.mk EVM_CONFIG=${EVM_CONFIG} evm_installer.wasm

FROM ${BASE_IMAGE}
COPY --from=kernel_build /build/*.wasm /kernel/
COPY --from=kernel_build /build/_evm_installer_preimages /kernel/_evm_installer_preimages
