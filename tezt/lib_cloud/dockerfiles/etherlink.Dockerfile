# This dockerfile can be run on any system and provides an image with
# the latest EVM node released and everything necessary for tezt-cloud.

ARG EVM_NODE_VERSION=latest
FROM tezos/tezos:octez-evm-node-${EVM_NODE_VERSION}

# hadolint ignore=DL3002
USER root

# hadolint ignore=DL3018
RUN apk add --no-cache \
    openssh openssh-server \
    docker-cli screen file \
    curl wget jq \
    prometheus prometheus-node-exporter \
    coreutils perl bash

# Install Foundry (cast) - download pre-built Alpine binaries (musl-compatible)
# Detect architecture and download appropriate binary (amd64 or arm64)
# hadolint ignore=DL3003,DL4006
RUN mkdir -p /tmp/foundry && cd /tmp/foundry && \
    ARCH=$(uname -m) && \
    if [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then \
      curl -L https://github.com/foundry-rs/foundry/releases/download/stable/foundry_stable_alpine_arm64.tar.gz | tar xz; \
    else \
      curl -L https://github.com/foundry-rs/foundry/releases/download/stable/foundry_stable_alpine_amd64.tar.gz | tar xz; \
    fi && \
    mv cast /usr/local/bin/ && \
    chmod +x /usr/local/bin/cast && \
    rm -rf /tmp/foundry

# hadolint ignore=DL3022
COPY --from=ncabatoff/process-exporter:latest \
    /bin/process-exporter /usr/local/bin/prometheus-process-exporter

# SSH key that will be used for the SSH server
ARG SSH_PUBLIC_KEY

# This is extracted from the link below
# https://dev.to/yakovlev_alexey/running-ssh-in-an-alpine-docker-container-3lop
RUN mkdir -p /root/.ssh \
    && chmod 0700 /root/.ssh \
    && echo "$SSH_PUBLIC_KEY" > /root/.ssh/authorized_keys \
    && ssh-keygen -A \
    && echo "PasswordAuthentication no" >> /etc/ssh/sshd_config \
    && mkdir -p /run/openrc \
    && touch /run/openrc/softlevel

# Path where binaries should be stored on the docker container
ARG BINARIES_DESTINATION_PATH
RUN mkdir -p $BINARIES_DESTINATION_PATH/ && \
    cp /usr/local/bin/octez-evm-node $BINARIES_DESTINATION_PATH/

CMD ["-D", "-p", "30000", "-e"]
ENTRYPOINT ["/usr/sbin/sshd"]
