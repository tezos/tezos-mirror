ARG IMAGE=invalid
# The image with proper version is set as ARG
#hadolint ignore=DL3006
FROM ${IMAGE}

#hadolint ignore=DL3008,DL3041
RUN dnf -y update && \
    dnf -y install systemd bash procps && \
    dnf clean all

#hadolint ignore=DL3045
COPY scripts/ci/systemd-entrypoint.service /etc/systemd/system/bash.service
#hadolint ignore=DL3045
COPY scripts/ci/systemd-entrypoint.sh /entrypoint.sh
#hadolint ignore=DL3045
COPY scripts/ci/octez-packages-version.sh scripts/ci/octez-packages-version.sh
#hadolint ignore=DL3045
COPY script-inputs/active_protocol_versions_without_number script-inputs/active_protocol_versions_without_number
#hadolint ignore=DL3045
COPY scripts/packaging/tests/tests-common.inc.sh scripts/packaging/tests/tests-common.inc.sh
RUN chown root:root /entrypoint.sh \
    && chmod 755 /entrypoint.sh \
    && chown root:root /etc/systemd/system/bash.service \
    && chmod 644 /etc/systemd/system/bash.service \
    && systemctl enable bash.service
ENTRYPOINT ["/entrypoint.sh"]
