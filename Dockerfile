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
# Add entrypoint scripts
COPY --chown=tezos:nogroup scripts/docker/entrypoint.* /home/tezos/bin/
# Add scripts
COPY --chown=tezos:nogroup scripts/alphanet_version scripts/tezos-docker-manager.sh src/bin_client/bash-completion.sh active_protocol_versions /home/tezos/scripts/

# Although alphanet.sh has been replaced by tezos-docker-manager.sh,
# the built-in auto-update mechanism expects an alphanet.sh script to exist.
# So we keep it for a while as a symbolic link.
CMD ["ln", "-s", "tezos-docker-manager.sh", "/home/tezos/scripts/alphanet.sh"]

FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as debug
ARG BUILD_IMAGE
ARG BUILD_IMAGE_VERSION
ARG COMMIT_SHORT_SHA
LABEL maintainer="contact@nomadic-labs.com" \
      org.label-schema.name="Tezos" \
      org.label-schema.docker.schema-version="1.0" \
      org.label-schema.description="Tezos node" \
      org.label-schema.url="https://www.nomadic-labs.com" \
      org.label-schema.vcs-url="https://gitlab.com/tezos/tezos" \
      org.label-schema.vcs-ref="${COMMIT_SHORT_SHA}" \
      org.label-schema.build-image="${BUILD_IMAGE}:${BUILD_IMAGE_VERSION}"

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
LABEL maintainer="contact@nomadic-labs.com" \
      org.label-schema.name="Tezos" \
      org.label-schema.docker.schema-version="1.0" \
      org.label-schema.description="Tezos node" \
      org.label-schema.url="https://www.nomadic-labs.com" \
      org.label-schema.vcs-url="https://gitlab.com/tezos/tezos" \
      org.label-schema.vcs-ref="${COMMIT_SHORT_SHA}" \
      org.label-schema.build-image="${BUILD_IMAGE}:${BUILD_IMAGE_VERSION}"
COPY --chown=tezos:nogroup --from=stripper /home/tezos/bin/ /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/scripts/ /usr/local/share/tezos


FROM  ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as minimal
ARG BUILD_IMAGE
ARG BUILD_IMAGE_VERSION
ARG COMMIT_SHORT_SHA
LABEL maintainer="contact@nomadic-labs.com" \
      org.label-schema.name="Tezos" \
      org.label-schema.docker.schema-version="1.0" \
      org.label-schema.description="Tezos node" \
      org.label-schema.url="https://www.nomadic-labs.com" \
      org.label-schema.vcs-url="https://gitlab.com/tezos/tezos" \
      org.label-schema.vcs-ref="${COMMIT_SHORT_SHA}" \
      org.label-schema.build-image="${BUILD_IMAGE}:${BUILD_IMAGE_VERSION}"

COPY --chown=tezos:nogroup --from=stripper /home/tezos/bin/ /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/bin/entrypoint.* /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/scripts/ /usr/local/share/tezos
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
