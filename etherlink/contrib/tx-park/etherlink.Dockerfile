ARG TEZOS_IMAGE=tezos/tezos-bare
ARG TEZOS_TAG=master

ARG RUST_IMAGE=us-central1-docker.pkg.dev/nl-gitlab-runner/protected-registry/tezos/tezos/rust-toolchain
ARG RUST_TAG=master

FROM ${TEZOS_IMAGE}:${TEZOS_TAG} AS kernel_setup_builder

ARG SEQUENCER_SK
ARG ADMIN_SK
ARG SMART_ROLLUP_BATCHER1_SK
ARG SMART_ROLLUP_BATCHER2_SK
ARG SMART_ROLLUP_BATCHER3_SK

ARG KERNEL_CONTRACT_ADMIN
ARG KERNEL_CONTRACT_DELAYED_BRIDGE
ARG KERNEL_CONTRACT_TICKETER

ARG KERNEL_CHAIN_ID
ARG KERNEL_DA_FEE_PER_BYTE
ARG KERNEL_DAL_SLOTS
ARG KERNEL_DELAYED_INBOX_MIN_LEVELS
ARG KERNEL_DELAYED_INBOX_TIMEOUT
ARG KERNEL_ENABLE_DAL
ARG KERNEL_ENABLE_FA_BRIDGE
ARG KERNEL_ENABLE_FAST_FA_WITHDRAWAL
ARG KERNEL_ENABLE_FAST_WITHDRAWAL
ARG KERNEL_ENABLE_MULTICHAIN
ARG KERNEL_ETH_BOOTSTRAP_ACCOUNT1
ARG KERNEL_ETH_BOOTSTRAP_ACCOUNT2
ARG KERNEL_ETH_BOOTSTRAP_ACCOUNT3
ARG KERNEL_ETH_BOOTSTRAP_ACCOUNT4
ARG KERNEL_ETH_BOOTSTRAP_ACCOUNT5
ARG KERNEL_ETH_BOOTSTRAP_ACCOUNT6
ARG KERNEL_ETH_BOOTSTRAP_BALANCE
ARG KERNEL_EVM_VERSION
ARG KERNEL_GOVERNANCE
ARG KERNEL_ROOT_HASH
ARG KERNEL_SECURITY_GOVERNANCE
ARG KERNEL_L2_CHAIN_ID
ARG KERNEL_MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS
ARG KERNEL_MAX_DELAYED_INBOX_BLUEPRINT_LENGTH
ARG KERNEL_MAXIMUM_ALLOWED_TICKS
ARG KERNEL_MAXIMUM_GAS_PER_TRANSACTION
ARG KERNEL_MINIMUM_BASE_FEE_PER_GAS
ARG KERNEL_SEQUENCER
ARG KERNEL_SEQUENCER_GOVERNANCE
ARG KERNEL_SEQUENCER_POOL_ADDRESS
ARG KERNEL_SET_CODE1
ARG KERNEL_SET_CODE2
ARG KERNEL_SET_CODE3
ARG KERNEL_SET_CODE4
ARG KERNEL_SET_CODE5
ARG KERNEL_SET_CODE6

WORKDIR /build/

COPY --chown=tezos etherlink/tezos_contracts/ etherlink/tezos_contracts/

RUN sudo apk add --no-cache --update jq curl bash

RUN mkdir client

RUN octez-client -d client -E https://rpc.tzkt.io/ghostnet import secret key \
    rollup-batcher1 ${SMART_ROLLUP_BATCHER1_SK}

RUN octez-client -d client -E https://rpc.tzkt.io/ghostnet import secret key \
    rollup-batcher2 ${SMART_ROLLUP_BATCHER2_SK}

RUN octez-client -d client -E https://rpc.tzkt.io/ghostnet import secret key \
    rollup-batcher3 ${SMART_ROLLUP_BATCHER3_SK}

RUN octez-client -d client -E https://rpc.tzkt.io/ghostnet import secret key \
    rollup-sequencer ${SEQUENCER_SK}

RUN octez-client -d client -E https://rpc.tzkt.io/ghostnet import secret key \
    rollup-admin ${ADMIN_SK}

RUN if [ -z "${KERNEL_CONTRACT_TICKETER}" ]; then \
    octez-client -d client -E https://rpc.tzkt.io/ghostnet originate contract ticketer \
    transferring 0 from rollup-admin running etherlink/tezos_contracts/exchanger.tz \
    --init 'Unit' --burn-cap 1; \
    fi

RUN if [ -z "${KERNEL_CONTRACT_DELAYED_BRIDGE}" ]; then \
    octez-client -d client -E https://rpc.tzkt.io/ghostnet originate contract delayed_bridge \
    transferring 0 from rollup-admin running \
    etherlink/tezos_contracts/chunked_delayed_transaction_bridge.tz \
    --init 'Unit' --burn-cap 1; \
    fi

RUN if [ -z "${KERNEL_CONTRACT_ADMIN}" ]; then \
    ADMIN_PKH=$(jq -r '.[] | select(.name == "rollup-admin") | .value' client/public_key_hashs) && \
    octez-client -d client -E https://rpc.tzkt.io/ghostnet originate contract admin \
    transferring 0 from rollup-admin running etherlink/tezos_contracts/admin.tz \
    --init "\"$ADMIN_PKH\"" --burn-cap 1; \
    fi

RUN KERNEL_SEQUENCER=${KERNEL_SEQUENCER:-$(jq -r '.[] | select(.name == "rollup-sequencer") | .value.key' client/public_keys)} && \
    KERNEL_CONTRACT_DELAYED_BRIDGE=${KERNEL_CONTRACT_DELAYED_BRIDGE:-$(jq -r '.[] | select(.name == "delayed_bridge") | .value' client/contracts)} && \
    KERNEL_CONTRACT_TICKETER=${KERNEL_CONTRACT_TICKETER:-$(jq -r '.[] | select(.name == "ticketer") | .value' client/contracts)} && \
    KERNEL_CONTRACT_ADMIN=${KERNEL_CONTRACT_ADMIN:-$(jq -r '.[] | select(.name == "admin") | .value' client/contracts)} && \
    octez-evm-node make kernel installer config kernel_setup.yml \
    --sequencer ${KERNEL_SEQUENCER} \
    --admin ${KERNEL_CONTRACT_ADMIN} \
    --delayed-bridge ${KERNEL_CONTRACT_DELAYED_BRIDGE} \
    --ticketer ${KERNEL_CONTRACT_TICKETER} \
    ${KERNEL_CHAIN_ID} \
    ${KERNEL_DA_FEE_PER_BYTE} \
    ${KERNEL_DAL_SLOTS} \
    ${KERNEL_DELAYED_INBOX_MIN_LEVELS} \
    ${KERNEL_DELAYED_INBOX_TIMEOUT} \
    ${KERNEL_ENABLE_DAL} \
    ${KERNEL_ENABLE_FA_BRIDGE} \
    ${KERNEL_ENABLE_FAST_FA_WITHDRAWAL} \
    ${KERNEL_ENABLE_FAST_WITHDRAWAL} \
    ${KERNEL_ENABLE_MULTICHAIN} \
    ${KERNEL_ETH_BOOTSTRAP_ACCOUNT1} \
    ${KERNEL_ETH_BOOTSTRAP_ACCOUNT2} \
    ${KERNEL_ETH_BOOTSTRAP_ACCOUNT3} \
    ${KERNEL_ETH_BOOTSTRAP_ACCOUNT4} \
    ${KERNEL_ETH_BOOTSTRAP_ACCOUNT5} \
    ${KERNEL_ETH_BOOTSTRAP_ACCOUNT6} \
    ${KERNEL_ETH_BOOTSTRAP_BALANCE} \
    ${KERNEL_EVM_VERSION} \
    ${KERNEL_GOVERNANCE} \
    ${KERNEL_ROOT_HASH} \
    ${KERNEL_SECURITY_GOVERNANCE} \
    ${KERNEL_L2_CHAIN_ID} \
    ${KERNEL_MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS} \
    ${KERNEL_MAX_DELAYED_INBOX_BLUEPRINT_LENGTH} \
    ${KERNEL_MAXIMUM_ALLOWED_TICKS} \
    ${KERNEL_MAXIMUM_GAS_PER_TRANSACTION} \
    ${KERNEL_MINIMUM_BASE_FEE_PER_GAS} \
    ${KERNEL_SEQUENCER_POOL_ADDRESS} \
    ${KERNEL_SET_CODE1} \
    ${KERNEL_SET_CODE2} \
    ${KERNEL_SET_CODE3} \
    ${KERNEL_SET_CODE4} \
    ${KERNEL_SET_CODE5} \
    ${KERNEL_SET_CODE6}

FROM ${RUST_IMAGE}:${RUST_TAG} AS kernel_builder

WORKDIR /build

COPY --from=kernel_setup_builder /build/kernel_setup.yml /build/kernel_setup.yml

COPY kernels.mk etherlink.mk /build/
COPY src/kernel_sdk /build/src/kernel_sdk
COPY etherlink /build/etherlink
COPY sdk /build/sdk

RUN make -f etherlink.mk build-deps

RUN make --no-print-directory -f etherlink.mk \
    kernel_setup.yml DISPLAY_ROOT_HASH=true \
    evm_installer.wasm > root_hash

RUN cargo install tezos-smart-rollup-installer

RUN smart-rollup-installer get-reveal-installer\
    -S kernel_setup.yml\
    --upgrade-to evm_kernel.wasm\
    --output installer.hex\
    --preimages-dir wasm_2_0_0\
    -d

FROM kernel_setup_builder AS builder

COPY --from=kernel_builder /build/root_hash /build/app/root_hash
COPY --from=kernel_builder /build/installer.hex /build/app/installer.hex
COPY --from=kernel_builder /build/wasm_2_0_0/ /build/app/wasm_2_0_0/
COPY etherlink/contrib/tx-park/config/ /build/app/config/

WORKDIR /build

RUN octez-client -d client -E https://rpc.tzkt.io/ghostnet originate smart rollup smart_rollup \
    from rollup-admin of kind wasm_2_0_0 \
    of type '(or (or (pair bytes (ticket (pair nat (option bytes)))) bytes) bytes)' \
    with kernel file:app/installer.hex --burn-cap 2
