ARG BASE_IMAGE
ARG BASE_IMAGE_VERSION
ARG BASE_IMAGE_VERSION_NON_MIN
ARG BUILD_IMAGE
ARG BUILD_IMAGE_VERSION

FROM ${BUILD_IMAGE}:${BUILD_IMAGE_VERSION} as builder


FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as intermediate
# Pull in built binaries
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-baker-* /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-accuser-* /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-client /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-admin-client /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-node /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-proxy-server /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-signer /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-tx-rollup-* /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-sc-rollup-* /home/tezos/bin/
# Add entrypoint scripts
COPY --chown=tezos:nogroup scripts/docker/entrypoint.* /home/tezos/bin/
# Add scripts
COPY --chown=tezos:nogroup scripts/alphanet_version src/bin_client/bash-completion.sh active_protocol_versions /home/tezos/scripts/

FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as debug
ARG BUILD_IMAGE
ARG BUILD_IMAGE_VERSION
ARG COMMIT_SHORT_SHA

# Open Container Initiative
# https://github.com/opencontainers/image-spec/blob/main/annotations.md
LABEL org.opencontainers.image.authors="contact@nomadic-labs.com" \
      org.opencontainers.image.base.name="alpine:3.14" \
      org.opencontainers.image.description="Tezos node" \
      org.opencontainers.image.documentation="https://tezos.gitlab.io/" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://gitlab.com/tezos/tezos" \
      org.opencontainers.image.title="tezos-debug" \
      org.opencontainers.image.url="https://gitlab.com/tezos/tezos" \
      org.opencontainers.image.vendor="Nomadic Labs"

USER root
# hadolint ignore=DL3018
RUN apk --no-cache add vim
USER tezos

ENV EDITOR=/usr/bin/vi
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/bin/ /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/scripts/ /usr/local/share/tezos/
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]


FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION_NON_MIN} as stripper
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/bin/tezos-* /home/tezos/bin/
RUN chmod +rw /home/tezos/bin/tezos* && strip /home/tezos/bin/tezos*


FROM  ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as bare
ARG BUILD_IMAGE
ARG BUILD_IMAGE_VERSION
ARG COMMIT_SHORT_SHA

# Open Container Initiative
# https://github.com/opencontainers/image-spec/blob/main/annotations.md
LABEL org.opencontainers.image.authors="contact@nomadic-labs.com" \
      org.opencontainers.image.base.name="alpine:3.14" \
      org.opencontainers.image.description="Tezos node" \
      org.opencontainers.image.documentation="https://tezos.gitlab.io/" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://gitlab.com/tezos/tezos" \
      org.opencontainers.image.title="tezos-bare" \
      org.opencontainers.image.url="https://gitlab.com/tezos/tezos" \
      org.opencontainers.image.vendor="Nomadic Labs"

COPY --chown=tezos:nogroup --from=stripper /home/tezos/bin/ /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/scripts/ /usr/local/share/tezos


FROM  ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as minimal
ARG BUILD_IMAGE
ARG BUILD_IMAGE_VERSION
ARG COMMIT_SHORT_SHA

# Open Container Initiative
# https://github.com/opencontainers/image-spec/blob/main/annotations.md
LABEL org.opencontainers.image.authors="contact@nomadic-labs.com" \
      org.opencontainers.image.base.name="alpine:3.14" \
      org.opencontainers.image.description="Tezos node" \
      org.opencontainers.image.documentation="https://tezos.gitlab.io/" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://gitlab.com/tezos/tezos" \
      org.opencontainers.image.title="tezos" \
      org.opencontainers.image.url="https://gitlab.com/tezos/tezos" \
      org.opencontainers.image.vendor="Nomadic Labs"

COPY --chown=tezos:nogroup --from=stripper /home/tezos/bin/ /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/bin/entrypoint.* /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/scripts/ /usr/local/share/tezos
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
