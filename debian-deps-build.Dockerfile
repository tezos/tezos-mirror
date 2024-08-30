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
COPY scripts/version.sh /tmp/
COPY --link scripts/install_build_deps.sh /root/tezos/scripts/
COPY --link scripts/install_build_deps.rust.sh /root/tezos/scripts/
COPY --link scripts/install_dal_trusted_setup.sh /root/tezos/scripts/
COPY --link scripts/version.sh /root/tezos/scripts/
COPY --link Makefile /root/tezos/
COPY --link opam/virtual/octez-deps.opam.locked /root/tezos/opam/virtual/
COPY --link opam /root/tezos/

WORKDIR /root/tezos
#hadolint ignore=SC2154, SC1091
RUN . /tmp/version.sh && \
    eval $(opam env) && \
    . "/root/.cargo/env" && \
    make build-deps && \
    scripts/install_dal_trusted_setup.sh
