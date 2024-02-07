FROM rust:1.66.0 AS kernel_build
RUN apt-get update
RUN apt-get install -y \
    make \
    libc-dev \
    git \
    wabt \
    clang
RUN rustup target add wasm32-unknown-unknown
RUN git clone -b master --single-branch https://gitlab.com/tezos/tezos.git /tezos
WORKDIR /tezos
ARG EVM_CHAIN_ID_CONFIG
RUN echo "${EVM_CHAIN_ID_CONFIG}" >> /tezos/etherlink/config/sequencer.yaml
RUN cat /tezos/etherlink/config/sequencer.yaml
RUN make -f /tezos/kernels.mk build-deps && make -f etherlink.mk build-deps sequencer.wasm

FROM alpine:3.14
COPY --from=kernel_build /tezos/sequencer.wasm /tezos/evm_kernel.wasm /kernel/
COPY --from=kernel_build /tezos/_evm_installer_preimages/ /kernel/_evm_installer_preimages
