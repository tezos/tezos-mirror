# Docker Buildx
# https://github.com/docker/buildx
ARG DOCKER_VERSION
FROM docker/buildx-bin:0.12.0 AS buildx

# Docker Alpine 3.16 based image
# https://hub.docker.com/_/docker
ARG DOCKER_VERSION
# hadolint ignore=DL3006
FROM docker:${DOCKER_VERSION} AS base

SHELL ["/bin/sh", "-eu", "-c"]

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8

# Upgrade base image and install tools
# hadolint ignore=DL3018,DL3019
RUN apk add \
    bash \
    ca-certificates \
    curl \
    git \
    jq \
    moreutils \
    python3 \
    libc6-compat \
    openssh-client \
    gnupg \
 && rm -rf /var/cache/apk/*

COPY --from=buildx /buildx /usr/libexec/docker/cli-plugins/docker-buildx

ARG TARGETARCH

# envsubst (Go version)
# https://github.com/a8m/envsubst
RUN if [ ${TARGETARCH} = "arm64" ]; then \
        curl -fsSL "https://github.com/a8m/envsubst/releases/download/v1.2.0/envsubst-Linux-arm64" -o /usr/local/bin/envsubst; \
    else \
        curl -fsSL "https://github.com/a8m/envsubst/releases/download/v1.2.0/envsubst-Linux-x86_64" -o /usr/local/bin/envsubst; \
    fi; \
    chmod 755 /usr/local/bin/envsubst

# AWS ECR credential helper
# https://github.com/awslabs/amazon-ecr-credential-helper
RUN curl -fsSL "https://amazon-ecr-credential-helper-releases.s3.us-east-2.amazonaws.com/0.6.0/linux-${TARGETARCH}/docker-credential-ecr-login" \
          -o /usr/local/bin/docker-credential-ecr-login \
 && chmod 755 /usr/local/bin/docker-credential-ecr-login

# Client interface for the Docker registry API
# https://github.com/regclient/regclient
RUN curl -fsSL "https://github.com/regclient/regclient/releases/download/v0.4.3/regctl-linux-${TARGETARCH}" \
          -o /usr/local/bin/regctl \
 && chmod 755 /usr/local/bin/regctl
RUN curl -fsSL "https://github.com/regclient/regclient/releases/download/v0.4.3/regsync-linux-${TARGETARCH}" \
          -o /usr/local/bin/regsync \
 && chmod 755 /usr/local/bin/regsync

# Dockerfile linter
# https://github.com/hadolint/hadolint
RUN if [ ${TARGETARCH} = "arm64" ]; then \
        curl -fsSL "https://github.com/hadolint/hadolint/releases/download/v2.10.0/hadolint-Linux-arm64" -o /usr/local/bin/hadolint; \
    else \
        curl -fsSL "https://github.com/hadolint/hadolint/releases/download/v2.10.0/hadolint-Linux-x86_64" -o /usr/local/bin/hadolint; \
    fi; \
    chmod 755 /usr/local/bin/hadolint

# Download and install the Google Cloud SDK
ENV GCLOUD_VERSION=472.0.0
RUN curl -O https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-${GCLOUD_VERSION}-linux-x86_64.tar.gz && \
    tar xzf google-cloud-sdk-${GCLOUD_VERSION}-linux-x86_64.tar.gz && \
    rm google-cloud-sdk-${GCLOUD_VERSION}-linux-x86_64.tar.gz

ENV PATH=/google-cloud-sdk/bin:$PATH

# Initialize the Google Cloud SDK
RUN gcloud components install core gsutil && \
    gcloud components install kubectl
