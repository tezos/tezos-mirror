ARG BASE_IMAGE
ARG BASE_IMAGE_VERSION
ARG BASE_IMAGE_VERSION_NON_MIN
ARG BUILD_IMAGE
ARG BUILD_IMAGE_VERSION

FROM ${BUILD_IMAGE}:${BUILD_IMAGE_VERSION} as builder


FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as intermediate
# Pull in built binaries
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-baker-* /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-endorser-* /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-accuser-* /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-client /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-admin-client /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-node /home/tezos/bin/
COPY --chown=tezos:nogroup --from=builder /home/tezos/tezos/tezos-signer /home/tezos/bin/
# Add entrypoint scripts
COPY --chown=tezos:nogroup scripts/docker/entrypoint.* /home/tezos/bin/
# Add scripts
COPY --chown=tezos:nogroup scripts/alphanet_version scripts/alphanet.sh src/bin_client/bash-completion.sh active_protocol_versions /home/tezos/scripts/


FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as debug
RUN sudo apk --no-cache add vim
ENV EDITOR=/usr/bin/vi
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/bin/ /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/scripts/ /usr/local/share/tezos/
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]


FROM ${BASE_IMAGE}:${BASE_IMAGE_VERSION_NON_MIN} as stripper
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/bin/tezos-* /home/tezos/bin/
RUN strip /home/tezos/bin/tezos*


FROM  ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as bare
COPY --chown=tezos:nogroup --from=stripper /home/tezos/bin/ /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/scripts/ /usr/local/share/tezos


FROM  ${BASE_IMAGE}:${BASE_IMAGE_VERSION} as minimal
COPY --chown=tezos:nogroup --from=stripper /home/tezos/bin/ /usr/local/bin/
COPY --chown=tezos:nogroup --from=intermediate /home/tezos/scripts/ /usr/local/share/tezos
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
