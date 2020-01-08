ARG BASE_IMAGE=registry.gitlab.com/tezos/opam-repository
ARG BASE_IMAGE_VERSION
FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION}
WORKDIR /home/tezos
RUN mkdir -p /home/tezos/tezos/scripts
COPY --chown=tezos:nogroup Makefile tezos
COPY --chown=tezos:nogroup active_protocol_versions tezos
COPY --chown=tezos:nogroup scripts/version.sh tezos/scripts/
COPY --chown=tezos:nogroup src tezos/src
COPY --chown=tezos:nogroup vendors tezos/vendors
RUN opam exec -- make -C tezos all build-test
