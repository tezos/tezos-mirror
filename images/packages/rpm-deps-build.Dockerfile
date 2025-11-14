ARG IMAGE=invalid
# The image with proper version is set as ARG
#hadolint ignore=DL3006
FROM $IMAGE

ENV TZ=Etc/UTC
# Build blst used by ocaml-bls12-381 without ADX to support old CPU
# architectures.
# See https://gitlab.com/tezos/tezos/-/issues/1788 and
# https://gitlab.com/dannywillems/ocaml-bls12-381/-/merge_requests/135/
ENV BLST_PORTABLE=true
ENV VENV_PATH=$HOME/venv

ARG OPAM_VERSION
ENV OPAM_VERSION=${OPAM_VERSION}

#hadolint ignore=DL3041
RUN dnf -y update &&\
    dnf install -y rpmdevtools &&\
    dnf clean all

WORKDIR /root/tezos
COPY ./scripts/version.sh ./scripts/version.sh
COPY scripts/ci/bin_packages_rpm_dependencies.sh \
  ./scripts/ci/bin_packages_rpm_dependencies.sh
COPY images/scripts/install_sccache_static.sh \
     images/scripts/install_opam_static.sh \
     scripts/kiss-fetch.sh \
     scripts/kiss-logs.sh \
     /tmp/

RUN scripts/ci/bin_packages_rpm_dependencies.sh

# we trust sw distributors
# We install sccache as a static binary because at the moment of writing
RUN /tmp/install_sccache_static.sh && \
    /tmp/install_opam_static.sh

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
COPY --link scripts/version.sh /root/tezos/scripts/
COPY --link Makefile /root/tezos/
COPY --link opam/virtual/octez-deps.opam.locked /root/tezos/opam/virtual/
COPY --link opam/virtual/release-tools-deps.opam.locked /root/tezos/opam/virtual/
COPY --link opam /root/tezos/

WORKDIR /root/tezos

ENV KISSCACHE="http://kisscache.kisscache.svc.cluster.local"
ENV OPAMFETCH="/tmp/kiss-fetch.sh"

#hadolint ignore=SC2154, SC1091
RUN eval $(opam env) && \
    . "/root/.cargo/env" && \
    make build-deps && \
# print kisscache stats
    /tmp/kiss-logs.sh /tmp/kiss.log \
    && rm -f /tmp/kiss.log
