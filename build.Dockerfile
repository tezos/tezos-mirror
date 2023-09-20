ARG BASE_IMAGE=registry.gitlab.com/tezos/opam-repository
ARG BASE_IMAGE_VERSION
ARG RUST_TOOLCHAIN_IMAGE
ARG RUST_TOOLCHAIN_IMAGE_VERSION

FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as without-evm-artifacts
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
RUN mkdir -p /home/tezos/tezos/scripts /home/tezos/tezos/script-inputs /home/tezos/tezos/parameters /home/tezos/evm_kernel
COPY --chown=tezos:nogroup Makefile tezos
COPY --chown=tezos:nogroup script-inputs/active_protocol_versions tezos/script-inputs/
COPY --chown=tezos:nogroup script-inputs/active_protocol_versions_without_number tezos/script-inputs/
COPY --chown=tezos:nogroup script-inputs/released-executables tezos/script-inputs/
COPY --chown=tezos:nogroup script-inputs/experimental-executables tezos/script-inputs/
COPY --chown=tezos:nogroup dune tezos
COPY --chown=tezos:nogroup scripts/version.sh tezos/scripts/
COPY --chown=tezos:nogroup src tezos/src
COPY --chown=tezos:nogroup tezt tezos/tezt
COPY --chown=tezos:nogroup opam tezos/opam
COPY --chown=tezos:nogroup dune tezos/dune
COPY --chown=tezos:nogroup dune-workspace tezos/dune-workspace
COPY --chown=tezos:nogroup dune-project tezos/dune-project
COPY --chown=tezos:nogroup vendors tezos/vendors
ENV GIT_SHORTREF=${GIT_SHORTREF}
ENV GIT_DATETIME=${GIT_DATETIME}
ENV GIT_VERSION=${GIT_VERSION}
RUN opam exec -- make -C tezos release OCTEZ_EXECUTABLES="${OCTEZ_EXECUTABLES}" OCTEZ_BIN_DIR=bin
# Gather the parameters of all active protocols in 1 place
RUN while read -r protocol; do \
    mkdir -p tezos/parameters/"$protocol"-parameters && \
    cp tezos/src/proto_"$(echo "$protocol" | tr - _)"/parameters/*.json tezos/parameters/"$protocol"-parameters; \
    done < tezos/script-inputs/active_protocol_versions

FROM ${RUST_TOOLCHAIN_IMAGE}:${RUST_TOOLCHAIN_IMAGE_VERSION} AS layer2-builder
WORKDIR /home/tezos/
RUN mkdir -p /home/tezos/evm_kernel
COPY --chown=tezos:nogroup kernels.mk evm_kernel
COPY --chown=tezos:nogroup src evm_kernel/src
RUN make -C evm_kernel -f kernels.mk build-deps \
  && make -C evm_kernel -f kernels.mk EVM_CONFIG=src/kernel_evm/config/dailynet.yaml evm_installer.wasm

# We move the EVM kernel in the final image in a dedicated stage to parallelize
# the two builder stages.
FROM without-evm-artifacts as with-evm-artifacts
COPY --from=layer2-builder --chown=tezos:nogroup /home/tezos/evm_kernel/evm_installer.wasm evm_kernel
COPY --from=layer2-builder --chown=tezos:nogroup /home/tezos/evm_kernel/_evm_installer_preimages/ evm_kernel/_evm_installer_preimages
