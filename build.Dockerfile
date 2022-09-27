ARG BASE_IMAGE=registry.gitlab.com/tezos/opam-repository
ARG BASE_IMAGE_VERSION
FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION}
# use alpine /bin/ash and set pipefail.
# see https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#run
SHELL ["/bin/ash", "-o", "pipefail", "-c"]
# do not move the ARG below above the FROM or it gets erased
ARG GIT_SHORTREF
ARG GIT_DATETIME
ARG GIT_VERSION
WORKDIR /home/tezos
RUN mkdir -p /home/tezos/tezos/scripts /home/tezos/tezos/script-inputs /home/tezos/tezos/parameters
COPY --chown=tezos:nogroup Makefile tezos
COPY --chown=tezos:nogroup script-inputs/active_protocol_versions tezos/script-inputs/
COPY --chown=tezos:nogroup script-inputs/tx_rollup_protocol_versions tezos/script-inputs/
COPY --chown=tezos:nogroup script-inputs/sc_rollup_protocol_versions tezos/script-inputs/
COPY --chown=tezos:nogroup dune tezos
COPY --chown=tezos:nogroup scripts/version.sh tezos/scripts/
COPY --chown=tezos:nogroup src tezos/src
COPY --chown=tezos:nogroup opam tezos/opam
COPY --chown=tezos:nogroup dune tezos/dune
COPY --chown=tezos:nogroup dune-workspace tezos/dune-workspace
COPY --chown=tezos:nogroup dune-project tezos/dune-project
COPY --chown=tezos:nogroup vendors tezos/vendors
ENV GIT_SHORTREF=${GIT_SHORTREF}
ENV GIT_DATETIME=${GIT_DATETIME}
ENV GIT_VERSION=${GIT_VERSION}
RUN opam exec -- make -C tezos release
# Gather the parameters of all active protocols in 1 place
RUN while read -r protocol; do \
    mkdir -p tezos/parameters/"$protocol"-parameters && \
    cp tezos/src/proto_"$(echo "$protocol" | tr - _)"/parameters/*.json tezos/parameters/"$protocol"-parameters; \
    done < tezos/script-inputs/active_protocol_versions
