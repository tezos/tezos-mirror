ARG OCTEZ_TAG
FROM tezos/tezos-bare:${OCTEZ_TAG} AS tezos_with_curl
USER root
RUN apk add curl
USER tezos
