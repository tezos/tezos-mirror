ARG IMAGE=invalid
# The image with proper version is set as ARG
#hadolint ignore=DL3006
FROM ${IMAGE}

#hadolint ignore=DL3008
RUN apt-get update && \
    apt-get install -y systemd --no-install-recommends && \
    rm -Rf /var/lib/apt/lists/*
#hadolint ignore=DL3045
COPY scripts/ci/systemd-entrypoint.service /etc/systemd/system/bash.service
#hadolint ignore=DL3045
COPY scripts/ci/systemd-entrypoint.sh /entrypoint.sh
#hadolint ignore=DL3045
COPY scripts/ci/octez-packages-version.sh scripts/ci/octez-packages-version.sh
#hadolint ignore=DL3045
COPY scripts/packaging/tests/tests-common.inc.sh scripts/packaging/tests/tests-common.inc.sh
#hadolint ignore=DL3045
COPY script-inputs/active_protocol_versions_without_number script-inputs/active_protocol_versions_without_number
RUN chown root:root /entrypoint.sh \
    && chmod 755 /entrypoint.sh \
    && chown root:root /etc/systemd/system/bash.service \
    && chmod 644 /etc/systemd/system/bash.service \
    && systemctl enable bash.service
ENTRYPOINT ["/entrypoint.sh"]
