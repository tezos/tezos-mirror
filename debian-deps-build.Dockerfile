ARG IMAGE
FROM ${IMAGE}

ARG DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC

RUN apt update && \
    apt-get install -y bubblewrap \
      rsync git m4 build-essential \
      patch unzip wget opam jq bc \
      autoconf cmake libev-dev \
      libffi-dev libgmp-dev \
      libhidapi-dev pkg-config \
      zlib1g-dev debhelper debconf \
      libprotobuf-dev protobuf-compiler \
      libsqlite3-dev && \
    apt-get clean

COPY scripts/version.sh /tmp/
RUN wget https://sh.rustup.rs/rustup-init.sh && \
    . /tmp/version.sh && \
    chmod +x rustup-init.sh && \
    ./rustup-init.sh --profile minimal \
      --default-toolchain $recommended_rust_version -y

RUN opam init --bare --disable-sandboxing
# we do not need everything to run build-deps
# we copy the mininum amount of files to use
# the caching mechanism more efficiently
COPY --link scripts/install_build_deps.sh root/tezos/scripts/
COPY --link scripts/install_build_deps.raw.sh root/tezos/scripts/
COPY --link scripts/install_build_deps.rust.sh root/tezos/scripts/
COPY --link scripts/version.sh root/tezos/scripts/
COPY --link Makefile root/tezos/
COPY --link opam/virtual/octez-deps.opam root/tezos/opam/virtual/
COPY --link opam root/tezos/

WORKDIR root/tezos
RUN . /tmp/version.sh && \
    echo $opam_repository_tag > opam_repository_tag
RUN eval $(opam env) ; \
    . $HOME/.cargo/env ; \
    make build-deps
