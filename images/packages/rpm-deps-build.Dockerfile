ARG IMAGE
# The image with proper version is set as ARG
#hadolint ignore=DL3006
FROM ${IMAGE}

ENV TZ=Etc/UTC
# Build blst used by ocaml-bls12-381 without ADX to support old CPU
# architectures.
# See https://gitlab.com/tezos/tezos/-/issues/1788 and
# https://gitlab.com/dannywillems/ocaml-bls12-381/-/merge_requests/135/
ENV BLST_PORTABLE=true
ENV VENV_PATH=$HOME/venv

#hadolint ignore=DL3041
RUN dnf -y update &&\
    dnf install -y rpmdevtools &&\
    dnf clean all

WORKDIR /root/tezos
COPY ./scripts/version.sh ./scripts/version.sh
COPY ./scripts/pkg-common/install_opam.sh ./scripts/pkg-common/install_opam.sh
COPY scripts/ci/bin_packages_rpm_dependencies.sh \
  ./scripts/ci/bin_packages_rpm_dependencies.sh
RUN scripts/ci/bin_packages_rpm_dependencies.sh

# we trust sw distributors
# We install sccache as a static binary because at the moment of writing
# the package sccache is not available on ubuntu jammy
#hadolint ignore=DL3008,DL3009
RUN ARCH=$(uname -m) && \
    curl  -L --output sccache.tgz "https://github.com/mozilla/sccache/releases/download/v0.8.1/sccache-v0.8.1-$ARCH-unknown-linux-musl.tar.gz" && \
    tar zxvf sccache.tgz && \
    cp "sccache-v0.8.1-$ARCH-unknown-linux-musl/sccache" /usr/local/bin/sccache && \
    rm -Rf sccache*

#hadolint ignore=SC2154
RUN . ./scripts/version.sh && \
    curl -s https://sh.rustup.rs > rustup-init.sh && \
    chmod +x rustup-init.sh && \
    ./rustup-init.sh --profile minimal \
      --default-toolchain "$recommended_rust_version" -y

RUN opam init --bare --disable-sandboxing

# We do not need everything to run build-deps
# We copy the mininum amount of files to use
# the caching mechanism more efficiently
COPY --link scripts/install_build_deps.sh /root/tezos/scripts/
COPY --link scripts/install_build_deps.rust.sh /root/tezos/scripts/
COPY --link scripts/install_dal_trusted_setup.sh /root/tezos/scripts/
COPY --link scripts/version.sh /root/tezos/scripts/
COPY --link Makefile /root/tezos/
COPY --link opam/virtual/octez-deps.opam.locked /root/tezos/opam/virtual/
COPY --link opam /root/tezos/

WORKDIR /root/tezos

# We download and copy the zcash params in a separate directory
# and before the make build-deps to create a second docker layer and
# optimize caching
RUN DAL_TRUSTED_SETUP="/root/tezos/dal-trusted-setup" \
    scripts/install_dal_trusted_setup.sh

#hadolint ignore=SC2154, SC1091
RUN eval $(opam env) && \
    . "/root/.cargo/env" && \
    make build-deps && \
    mv dal-trusted-setup _opam/share/dal-trusted-setup
