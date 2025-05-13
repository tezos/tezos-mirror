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

# we trust sw distributors
# We install sccache as a static binary because at the moment of writing
# the package sccache is not available on ubuntu jammy
#hadolint ignore=DL3008,DL3009
RUN apt-get update && \
    ARCH=$(uname -m) && \
    case "$ARCH" in \
        x86_64) export PLATFORM="x64" ;; \
        aarch64) export PLATFORM="arm64" ;; \
        *) echo "Unsupported architecture: $ARCH" >&2; exit 1 ;; \
    esac && \
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
    rm -rf /var/lib/apt/lists/* && \
    curl  -L --output sccache.tgz "https://github.com/mozilla/sccache/releases/download/v0.8.1/sccache-v0.8.1-$ARCH-unknown-linux-musl.tar.gz" && \
    tar zxvf sccache.tgz && \
    cp "sccache-v0.8.1-$ARCH-unknown-linux-musl/sccache" /usr/local/bin/sccache && \
    rm -Rf sccache* && \
    curl -L --fail "https://github.com/DataDog/datadog-ci/releases/download/v3.4.0/datadog-ci_linux-$PLATFORM" --output "/usr/local/bin/datadog-ci" && \
    chmod +x /usr/local/bin/datadog-ci

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
COPY --link opam/virtual/stdcompat.opam.locked /root/tezos/opam/virtual/
COPY --link opam /root/tezos/

WORKDIR /root/tezos

#hadolint ignore=SC2154, SC1091
RUN . ./scripts/version.sh && \
    eval $(opam env) && \
    . "/root/.cargo/env" && \
    make build-deps
