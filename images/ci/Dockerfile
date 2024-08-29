# runtime dependencies
#
# This image includes
# - alpine
# - runtime dependencies (libraries linked at load time of the process)
#
# This image is intended for
# - distributing the Tezos binaries in
# - building the CI images on top of it in the image stack (see README.md)

ARG BUILD_IMAGE
# hadolint ignore=DL3006
FROM ${BUILD_IMAGE} as runtime

# We must re-introduce BUILD_IMAGE in the scope to use it in the label.
ARG BUILD_IMAGE
LABEL org.opencontainers.image.base.name="${BUILD_IMAGE}"

# Use alpine /bin/ash and set shell options
# See https://docs.docker.com/develop/develop-images/dockerfile_best-practices/#run
SHELL ["/bin/ash", "-euo", "pipefail", "-c"]

# Open Container Initiative
# https://github.com/opencontainers/image-spec/blob/main/annotations.md
LABEL org.opencontainers.image.authors="contact@nomadic-labs.com" \
      org.opencontainers.image.description="Octez - GitLab CI docker image" \
      org.opencontainers.image.documentation="https://tezos.gitlab.io/" \
      org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://gitlab.com/tezos/tezos" \
      org.opencontainers.image.title="runtime" \
      org.opencontainers.image.url="https://gitlab.com/tezos/tezos" \
      org.opencontainers.image.vendor="Nomadic Labs"

USER root

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

# Sapling parameters
COPY ./zcash-params/sapling-output.params ./zcash-params/sapling-spend.params /usr/share/zcash-params/

# hadolint ignore=DL3018
RUN apk --no-cache add \
    binutils \
    gcc \
    gmp \
    libgmpxx \
    hidapi \
    libc-dev \
    libev \
    libffi \
    # Used by 'scripts/install_dal_trusted_setup.sh' below
    curl \
    sudo \
    sqlite-libs \
    # Used by teztale
    openssl-libs-static \
    sqlite-dev \
    postgresql-dev

# Install dal SRS setup (need curl)
COPY scripts/install_dal_trusted_setup.sh scripts/version.sh /tmp/
RUN mkdir -p /usr/share/dal-trusted-setup && \
    DAL_TRUSTED_SETUP=/usr/share/dal-trusted-setup \
    sh /tmp/install_dal_trusted_setup.sh && \
    rm /tmp/install_dal_trusted_setup.sh /tmp/version.sh

USER tezos
ENV USER=tezos
WORKDIR /home/tezos

# runtime + prebuild dependencies
#
# This image builds upon the `runtime` image, see its
# header for details on its content.
#
# It adds upon the contents of `runtime`:
# - non-opam build-dependencies (rust dependencies)
# - cache for opam build-dependencies
#
# This image is intended for
# - testing the buildability of tezos opam packages
# - building images on top of it in the image stack (see README.md)

FROM runtime as prebuild

LABEL org.opencontainers.image.title="prebuild"

USER root

# SHELL already set in runtime

WORKDIR /tmp

# Automatically set if you use Docker buildx
ARG TARGETARCH

# why the git config???
COPY --chown=tezos:tezos .gitconfig /home/tezos/

# hadolint ignore=DL3018,DL3019
RUN apk update \
# Do not use apk --no-cache here because opam needs the cache.
# See https://github.com/ocaml/opam/issues/5186
 && apk add --no-cache \
    autoconf \
    automake \
    bash \
    build-base \
    ca-certificates \
    cargo \
    cmake \
    coreutils \
    eudev-dev \
    git \
    gmp-dev \
    jq \
    libev-dev \
    libffi-dev \
    libtool \
    linux-headers \
    m4 \
    ncurses-dev \
    openssh-client \
    openssl-dev \
    patch \
    perl \
    postgresql14-dev \
    rsync \
    tar \
    unzip \
    wget \
    xz \
    zlib-dev \
    zlib-static \
    libusb-dev \
    hidapi-dev \
    sccache \
    shfmt \
    upx \
    protobuf \
    protobuf-dev \
    sqlite-static \
    sqlite-dev \
 # Opam from Edge is Opam 2.2
 && apk add --no-cache \
    opam --repository=https://dl-cdn.alpinelinux.org/alpine/edge/community \
# Cleanup
 && rm -rf /tmp/*

USER tezos
WORKDIR /home/tezos

# Create an opam-repository directory that is a clone of the public opam repository
# filtered to only keep packages which we actually need.
COPY --chown=tezos:tezos scripts/create_opam_repo.sh scripts/version.sh octez-deps.opam.locked ./
# Run the script, then clean up; we only need the result of the script (the opam repository).
RUN ./create_opam_repo.sh \
  && rm create_opam_repo.sh \
  && rm octez-deps.opam.locked \
  && rm version.sh

# Needed by 'opam admin cache' below
WORKDIR /home/tezos/opam-repository

# Install OCaml
ARG OCAML_VERSION
# hadolint ignore=SC2046,DL4006
RUN opam init --disable-sandboxing --no-setup --yes \
              --compiler ocaml-base-compiler.${OCAML_VERSION} \
              tezos /home/tezos/opam-repository \
 && opam clean

# Add opam cache
RUN opam admin cache \
 && opam update \
 && opam clean

ENTRYPOINT [ "opam", "exec", "--" ]
CMD [ "/bin/sh" ]

# runtime + build dependencies
#
# This image builds upon the `prebuild` image,
# see its header for details on its content.
#
# It removes the `cache for opam build-dependencies` from that image, and adds:
# - opam build-dependencies
#
# This image is intended for
# - building tezos from source
# - building images on top of it in the image stack (see README.md)

FROM prebuild as build

LABEL org.opencontainers.image.title="build"

# SHELL already set in runtime

USER tezos
WORKDIR /home/tezos

# Build blst used by ocaml-bls12-381 without ADX to support old CPU
# architectures.
# See https://gitlab.com/tezos/tezos/-/issues/1788 and
# https://gitlab.com/dannywillems/ocaml-bls12-381/-/merge_requests/135/
ENV BLST_PORTABLE=yes

# hadolint ignore=SC2046,DL4006
RUN opam install --yes $(opam list --all --short | grep -v ocaml-option-) \
    && opam clean

# ENTRYPOINT and CMD already set in prebuild

# runtime + build + test dependencies
#
# This image builds upon the `build` image, see
# its header for details on its content.
#
# It adds upon the contents of `build`:
# - shellcheck
# - a python environment for building the octez documentation
# - nvm for javascript backend testing
#
# This image is intended for
# - running certain tests in the tezos/tezos repo (unit tests, shellcheck)
# - building the documentation
# - building images on top of it in the image stack (see README.md)


FROM build as test

LABEL org.opencontainers.image.title="test"

# SHELL already set in runtime

USER root

WORKDIR /tmp

# hadolint ignore=DL3018,SC2046
RUN apk --no-cache add \
        python3-dev \
        poetry \
        shellcheck

USER tezos
WORKDIR /home/tezos

### Javascript env setup as tezos user

# TODO: https://gitlab.com/tezos/tezos/-/issues/5026
# The js dependencies could be downloaded from tezos/tezos and installed
# here.

COPY --chown=tezos:tezos scripts/install-nvm.sh /tmp/install-nvm.sh
RUN /tmp/install-nvm.sh \
 && rm -rf /tmp/*

### Python setup

# Required to have poetry in the path in the CI
ENV PATH="/home/tezos/.local/bin:${PATH}"

# Copy poetry files to install the dependencies in the docker image
COPY --chown=tezos:tezos ./poetry.lock ./pyproject.toml ./

# Poetry will create the virtual environment in $(pwd)/.venv.
# The containers running this image can load the virtualenv with
# $(pwd)/.venv/bin/activate and do not require to run `poetry install`
# It speeds up the Tezos CI and simplifies the .gitlab-ci.yml file
# by avoiding duplicated poetry setup checks.
RUN poetry config virtualenvs.in-project true \
 && poetry install \
 && rm -rf /tmp/* /home/tezos/.cache/pip /home/tezos/.cache/pypoetry

# ENTRYPOINT and CMD already set in prebuild

# runtime + end-to-end test dependencies
#
# This image builds on the runtime dependencies and additionally includes:
#
# - ocamlformat and bisect-ppx-report, copied from the
#   build image.
# - eth-cli, installed from npm
# - some utilities used in the Tezt integration test suite: curl, git, file
# - some utilities used in the [unified_coverage] job: make, jq
#
# This image is intended for
# - Running end-to-end tests (mostly Tezt) in the tezos/tezos CI.

ARG OCAML_IMAGE
ARG BUILD_IMAGE

# hadolint ignore=DL3006
FROM runtime as e2etest

LABEL org.opencontainers.image.title="e2etest"

USER root

# SHELL already set in runtime

WORKDIR /tmp

# Automatically set if you use Docker buildx
ARG TARGETARCH

# Retrieve ocamlformat, used in the snoop tests.
ARG OCAML_VERSION
COPY --from=build \
    /home/tezos/.opam/ocaml-base-compiler.${OCAML_VERSION}/bin/ocamlformat \
    /home/tezos/.opam/ocaml-base-compiler.${OCAML_VERSION}/bin/bisect-ppx-report \
    /bin/

# TODO: https://gitlab.com/tezos/tezos/-/issues/5026
# We could install npm via nvm if we tackle this issue.
# In the meantime, removes nvm installed in test and
# install npm via apk.

# Fixing some ipv6 issues on the runner. Always prioritizing ipv4
ENV NODE_OPTIONS="--dns-result-order=ipv4first"

# We need curl since a bunch of tezt tests use curl.
# Same, some tests use [file].

# hadolint ignore=DL3018,DL3019
RUN apk update \
 && apk add --no-cache curl npm git file make jq \
 gcc clang lld ca-certificates build-base musl-dev libusb-dev linux-headers \
 # We need eth since e2e tests. This requires git as well.
 && npm install -g eth-cli@2.0.2 \
 # We need solc to compile solidity smart-contracts
 && npm install -g solc@0.8.26 \
 && PATH="/root/.cargo/bin:${PATH}" \
 && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | ash -s -- -y \
 && RUSTFLAGS="-C linker=clang -C link-arg=-fuse-ld=lld" cargo install \
 --git https://github.com/foundry-rs/foundry/ cast --profile release --force --locked \
 && cp /root/.cargo/bin/cast /bin \
 # Clean up
 && rm -rf /tmp/* && rm -rf /root/.cargo

USER tezos
WORKDIR /home/tezos
