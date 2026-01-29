ARG BASE_IMAGE
ARG BASE_IMAGE_VERSION
ARG RUST_TOOLCHAIN_IMAGE_NAME
ARG RUST_TOOLCHAIN_IMAGE_TAG

# hadolint ignore=DL3006
FROM ${BASE_IMAGE}/${BASE_IMAGE_VERSION} as without-evm-artifacts
# use alpine /bin/ash and set pipefail.
# see https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#run
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
# Do not move the ARG below above the FROM or it gets erased.
# More precisely: ARG above FROM can be used in the FROM itself, but nowhere else.
ARG OCTEZ_EXECUTABLES
ARG GIT_SHORTREF
ARG GIT_DATETIME
ARG GIT_VERSION
WORKDIR /home/tezos
RUN mkdir -p /home/tezos/tezos/scripts/ci /home/tezos/tezos/script-inputs /home/tezos/tezos/parameters /home/tezos/evm_kernel
COPY --chown=tezos:nogroup Makefile tezos
COPY --chown=tezos:nogroup script-inputs/active_protocol_versions tezos/script-inputs/
COPY --chown=tezos:nogroup script-inputs/active_protocol_versions_without_number tezos/script-inputs/
COPY --chown=tezos:nogroup script-inputs/released-executables tezos/script-inputs/
COPY --chown=tezos:nogroup script-inputs/experimental-executables tezos/script-inputs/
COPY --chown=tezos:nogroup script-inputs/dev-executables tezos/script-inputs/
COPY --chown=tezos:nogroup dune tezos
COPY --chown=tezos:nogroup scripts/version.sh tezos/scripts/
COPY --chown=tezos:nogroup scripts/custom-flags.sh tezos/scripts/
COPY --chown=tezos:nogroup scripts/ci/dune.sh tezos/scripts/ci/dune.sh
COPY --chown=tezos:nogroup src tezos/src
COPY --chown=tezos:nogroup sdk/rust tezos/sdk/rust
COPY --chown=tezos:nogroup irmin tezos/irmin
COPY --chown=tezos:nogroup brassaia tezos/brassaia
COPY --chown=tezos:nogroup data-encoding tezos/data-encoding
COPY --chown=tezos:nogroup etherlink tezos/etherlink
COPY --chown=tezos:nogroup tezt tezos/tezt
COPY --chown=tezos:nogroup opam tezos/opam
COPY --chown=tezos:nogroup dune tezos/dune
COPY --chown=tezos:nogroup dune-workspace tezos/dune-workspace
COPY --chown=tezos:nogroup dune-project tezos/dune-project
COPY --chown=tezos:nogroup vendors tezos/vendors
COPY --chown=tezos:nogroup rust-toolchain tezos/rust-toolchain
COPY --chown=tezos:nogroup websocket tezos/websocket
COPY --chown=tezos:nogroup lwt_domain tezos/lwt_domain
COPY --chown=tezos:nogroup resto tezos/resto
COPY --chown=tezos:nogroup prometheus tezos/prometheus
COPY --chown=tezos:nogroup efunc-core tezos/efunc-core
COPY --chown=tezos:nogroup teztale tezos/teztale
COPY --chown=tezos:nogroup contrib tezos/contrib
ENV GIT_SHORTREF=${GIT_SHORTREF}
ENV GIT_DATETIME=${GIT_DATETIME}

ENV GIT_VERSION=${GIT_VERSION}
RUN opam exec -- make -C tezos release OCTEZ_EXECUTABLES="${OCTEZ_EXECUTABLES}" OCTEZ_BIN_DIR=bin
# Gather the parameters of all active protocols in 1 place
RUN while read -r protocol; do \
    mkdir -p tezos/parameters/"$protocol"-parameters && \
    cp tezos/src/proto_"$(echo "$protocol" | tr - _)"/parameters/*.json tezos/parameters/"$protocol"-parameters; \
    done < tezos/script-inputs/active_protocol_versions

FROM ${RUST_TOOLCHAIN_IMAGE_NAME}:${RUST_TOOLCHAIN_IMAGE_TAG} AS layer2-builder
WORKDIR /home/tezos/
RUN mkdir -p /home/tezos/evm_kernel
COPY --chown=tezos:nogroup kernels.mk etherlink.mk evm_kernel/
COPY --chown=tezos:nogroup src evm_kernel/src
COPY --chown=tezos:nogroup sdk/rust evm_kernel/sdk/rust
COPY --chown=tezos:nogroup etherlink evm_kernel/etherlink
COPY --chown=tezos:nogroup contrib evm_kernel/contrib
RUN make -C evm_kernel -f etherlink.mk build-deps \
  && make -C evm_kernel -f etherlink.mk EVM_KERNEL_SKIP_BYTECODE=yes EVM_CONFIG=etherlink/config/dailynet.yaml evm_installer.wasm \
  && make -C evm_kernel -f etherlink.mk EVM_KERNEL_SKIP_BYTECODE=yes evm_benchmark_kernel.wasm

# We move the EVM kernel in the final image in a dedicated stage to parallelize
# the two builder stages.
FROM without-evm-artifacts as with-evm-artifacts
COPY --from=layer2-builder --chown=tezos:nogroup /home/tezos/evm_kernel/evm_installer.wasm evm_kernel
COPY --from=layer2-builder --chown=tezos:nogroup /home/tezos/evm_kernel/_evm_installer_preimages/ evm_kernel/_evm_installer_preimages
COPY --from=layer2-builder --chown=tezos:nogroup /home/tezos/evm_kernel/evm_benchmark_kernel.wasm evm_kernel
COPY --from=layer2-builder --chown=tezos:nogroup /home/tezos/evm_kernel/etherlink/config/benchmarking.yaml evm_kernel
COPY --from=layer2-builder --chown=tezos:nogroup /home/tezos/evm_kernel/etherlink/config/benchmarking_sequencer.yaml evm_kernel
