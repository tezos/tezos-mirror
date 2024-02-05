# runtime + end-to-end test dependencies
#
# This image builds on the runtime build test dependencies and additionally includes:
#
# - kaitai-struct-compiler installed from the source.
# - xxd.
# - java.

# This image is intended for
# - Running end-to-end semantic validation of automatically generated
#   kaitai struct files (.ksy files).

ARG BUILD_IMAGE

# hadolint ignore=DL3006
FROM ${BUILD_IMAGE}

LABEL org.opencontainers.image.title="client-libs-dependencies"

USER root
WORKDIR /tmp

# hadolint ignore=DL3018
RUN apk update && apk add --no-cache \
    # Install xxd that is required for input generation of e2e kaitai semantic validation.
    # tezos/tezos CI job 'kaitai_e2e_checks' requires coreutils mktemp
    xxd coreutils \
    # Required to clone "kaitai-struct-compiler"
    git \
    # Install Java for building and running "kaitai-struct-compiler".
    openjdk11 \
    # deps for nodejs/install-nvm.sh
    bash wget gcc make

# Install "kaitai-struct-compiler".
# Building "kaitai-struct-compiler" from source is described here:
#    - https://doc.kaitai.io/developers.html#_prerequisites
# This requires temporarily installing Scala Build Tool (sbt).
# Ignore warning from use of 'cd'.
ARG SBT_VERSION=1.9.7
COPY sbt-$SBT_VERSION.tgz.sha256 .
# hadolint ignore=DL3003
RUN wget -q https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz \
    && sha256sum --check sbt-${SBT_VERSION}.tgz.sha256 \
    && tar xfz sbt-${SBT_VERSION}.tgz -C /tmp \
    # Clone the repository.
    && git clone --recursive https://github.com/kaitai-io/kaitai_struct.git \
    # Change working directory to "kaitai_scruct/compiler" as specified here:
    #    - https://doc.kaitai.io/developers.html#_building_for_jvm
    && cd /tmp/kaitai_struct/compiler \
    # Build universal zip package as specified here:
    #    - https://doc.kaitai.io/developers.html#_building_for_jvm
    && /tmp/sbt/bin/sbt compilerJVM/universal:packageBin \
    # Unzip executable to "/usr/local".
    &&  unzip jvm/target/universal/kaitai-struct-compiler-0.11-SNAPSHOT.zip -d /usr/local \
    # Remove "kaitai-struct" repository and "sbt".
    && rm -rf /tmp/sbt-${SBT_VERSION}.tgz /tmp/sbt-${SBT_VERSION}.tgz.sha256 /tmp/kaitai_struct /tmp/sbt /root/.cache /root/.sbt
# Make "kaitai-struct-compiler" available as part of the "PATH".
ENV PATH="/usr/local/kaitai-struct-compiler-0.11-SNAPSHOT/bin:${PATH}"

# Create a static system group and system user (no password + login shell)
# Prepare sudo, ssh and run
RUN echo 'tezos:x:1000:tezos' >> /etc/group \
 && echo 'tezos:x:1000:1000:tezos:/home/tezos:/bin/sh' >> /etc/passwd \
 && echo 'tezos:!::0:::::' >> /etc/shadow \
 && mkdir -pv /home/tezos/.ssh /run/tezos/client /run/tezos/node \
 && chown -R tezos:tezos /home/tezos /run/tezos \
 && chmod 700 /home/tezos/.ssh \
 && mkdir -pv /etc/sudoers.d \
 && echo 'tezos ALL=(ALL:ALL) NOPASSWD:ALL' > /etc/sudoers.d/tezos \
 && chmod 440 /etc/sudoers.d/tezos

USER tezos
ENV USER=tezos
WORKDIR /home/tezos

COPY --chown=tezos:tezos install-nvm.sh /tmp/install-nvm.sh
RUN /tmp/install-nvm.sh && rm -rf /tmp/install-nvm.sh
