ARG RUST_IMAGE=us-central1-docker.pkg.dev/nl-gitlab-runner/protected-registry/tezos/tezos/rust-toolchain
ARG RUST_TAG=master
ARG BASE_IMAGE=debian:sid-slim

FROM ${RUST_IMAGE}:${RUST_TAG} AS kernel_build
ARG EVM_CONFIG=etherlink/config/dev.yaml
ARG CI_COMMIT_SHA
ENV EVM_KERNEL_SKIP_BYTECODE="yes"
WORKDIR /build
COPY kernels.mk etherlink.mk /build/
COPY src/kernel_sdk /build/src/kernel_sdk
COPY etherlink /build/etherlink
COPY contrib/mir /build/contrib/mir
COPY sdk /build/sdk
RUN make -f etherlink.mk build-deps
RUN make --no-print-directory -f etherlink.mk EVM_CONFIG=${EVM_CONFIG} CI_COMMIT_SHA=${CI_COMMIT_SHA} DISPLAY_ROOT_HASH=true evm_installer.wasm > root_hash

FROM ${BASE_IMAGE}
COPY --from=kernel_build /build/*.wasm /build/root_hash /kernel/
COPY --from=kernel_build /build/_evm_installer_preimages /kernel/_evm_installer_preimages
