ARG IMAGE=invalid
# the image with proper version is set as ARG
#hadolint ignore=DL3006
FROM ${IMAGE}

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
ENV BLST_PORTABLE=true

# Build blst used by ocaml-bls12-381 without ADX to support old CPU
# architectures.
# See https://gitlab.com/tezos/tezos/-/issues/1788 and
# https://gitlab.com/dannywillems/ocaml-bls12-381/-/merge_requests/135/
ENV BLST_PORTABLE=true

COPY --chown=tezos:tezos \
  images/scripts/install_sccache_static.sh \
  images/scripts/install_opam_static.sh \
  scripts/kiss-fetch.sh \
  scripts/kiss-logs.sh \
  /tmp/

# we trust sw distributors
# We install sccache as a static binary because at the moment of writing
# the package sccache is not available on ubuntu jammy
#hadolint ignore=DL3008,DL3009
RUN apt-get update && \
    apt-get install --no-install-recommends -y bubblewrap \
      rsync git m4 build-essential \
      patch unzip curl wget ca-certificates \
      opam jq bc cargo \
      autoconf cmake libev-dev \
      libffi-dev libgmp-dev \
      libhidapi-dev pkg-config \
      zlib1g-dev debhelper debconf \
      libprotobuf-dev protobuf-compiler \
      libsqlite3-dev libpq-dev \
      lintian devscripts && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*


ARG OPAM_VERSION
ENV OPAM_VERSION=${OPAM_VERSION}

RUN /tmp/install_sccache_static.sh && \
    /tmp/install_opam_static.sh

COPY --link scripts/version.sh /root/tezos/scripts/

#hadolint ignore=SC2154
RUN . /root/tezos/scripts/version.sh && \
    curl -s https://sh.rustup.rs > rustup-init.sh && \
    chmod +x rustup-init.sh && \
    ./rustup-init.sh --profile minimal \
      --default-toolchain "$recommended_rust_version" -y

RUN opam init --bare --disable-sandboxing

# we do not need everything to run build-deps
# we copy the mininum amount of files to use
# the caching mechanism more efficiently
COPY --link scripts/install_build_deps.sh /root/tezos/scripts/
COPY --link scripts/install_build_deps.rust.sh /root/tezos/scripts/
COPY --link Makefile /root/tezos/
COPY --link opam/virtual/octez-deps.opam.locked /root/tezos/opam/virtual/
COPY --link opam/virtual/release-tools-deps.opam.locked /root/tezos/opam/virtual/
COPY --link opam/virtual/dream-httpaf.opam.locked /root/tezos/opam/virtual/
COPY --link opam/virtual/dream.opam.locked /root/tezos/opam/virtual/
COPY --link opam /root/tezos/

WORKDIR /root/tezos

ENV KISSCACHE="http://kisscache.kisscache.svc.cluster.local"
ENV OPAMFETCH="/tmp/kiss-fetch.sh"

#hadolint ignore=SC2154, SC1091
RUN . ./scripts/version.sh && \
    eval $(opam env) && \
    . "/root/.cargo/env" && \
    make build-deps && \
# print kisscache stats
    /tmp/kiss-logs.sh /tmp/kiss.log \
    && rm -f /tmp/kiss.log
