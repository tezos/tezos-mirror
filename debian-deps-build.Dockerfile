ARG IMAGE
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
    ARCH=$(uname -m) && \
    curl  -L --output sccache.tgz "https://github.com/mozilla/sccache/releases/download/v0.8.1/sccache-v0.8.1-$ARCH-unknown-linux-musl.tar.gz" && \
    tar zxvf sccache.tgz && \
    cp "sccache-v0.8.1-$ARCH-unknown-linux-musl/sccache" /usr/local/bin/sccache && \
    rm -Rf sccache*

#hadolint ignore=SC2154
ARG RECOMMENDED_RUST_VERSION=
RUN curl -s https://sh.rustup.rs > rustup-init.sh && \
    chmod +x rustup-init.sh && \
    ./rustup-init.sh --profile minimal \
      --default-toolchain "$RECOMMENDED_RUST_VERSION" -y

RUN opam init --bare --disable-sandboxing

# we do not need everything to run build-deps
# we copy the mininum amount of files to use
# the caching mechanism more efficiently
COPY --link scripts/install_build_deps.sh /root/tezos/scripts/
COPY --link scripts/install_build_deps.rust.sh /root/tezos/scripts/
COPY --link scripts/install_dal_trusted_setup.sh /root/tezos/scripts/
COPY --link scripts/version.sh /root/tezos/scripts/
COPY --link Makefile /root/tezos/
COPY --link opam/virtual/octez-deps.opam.locked /root/tezos/opam/virtual/
COPY --link opam /root/tezos/

WORKDIR /root/tezos

# we download and copy the zcash params in a separate directory
# and before the make build-deps to create a second docker layer and
# optimize caching
RUN DAL_TRUSTED_SETUP="/root/tezos/dal-trusted-setup" \
    scripts/install_dal_trusted_setup.sh

#hadolint ignore=SC2154, SC1091
RUN . ./scripts/version.sh && \
    eval $(opam env) && \
    . "/root/.cargo/env" && \
    make build-deps && \
    mv dal-trusted-setup _opam/share/dal-trusted-setup
