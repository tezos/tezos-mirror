#!/bin/sh

# Build dependency images for debian based distributions

set -ex

mkdir -p scripts/ci/ocaml4_job/scripts
#shellcheck disable=SC2002
sed 's/ocaml_version=[0-9]\+\.[0-9]\+\.[0-9]\+/ocaml_version=4.14.2/' \
  scripts/version.sh \
  > scripts/ci/ocaml4_job/scripts/version.sh

cp Makefile scripts/ci/ocaml4_job/
cp scripts/install_build_deps.rust.sh \
  scripts/install_dal_trusted_setup.sh \
  scripts/ci/ocaml4_job/scripts/
cp -a opam scripts/ci/ocaml4_job/opam

# Remove stdcompat pin
sed "/opam pin add https:\/\/github.com\/thierry-martinez\/stdcompat.git -n -y/d" scripts/install_build_deps.sh \
  > scripts/ci/ocaml4_job/scripts/install_build_deps.sh

chmod +x scripts/ci/ocaml4_job/scripts/install_build_deps.sh

# Replace OCaml 5.2.1 constraints with OCaml 4.14.2
sed 's/"ocaml" { >= "5.2.1" & < "5.3.0" }/"ocaml" { >= "4.12.2" & < "4.13.0" }/' opam/octez-libs.opam \
  > scripts/ci/ocaml4_job/opam/octez-libs.opam

sed '/"base-domains" {= "base"}/d' opam/virtual/octez-deps.opam.locked |
  sed '/"base-nnp" {= "base"}/d' |
  sed 's/"ocaml\([-a-z]*\)" {= "5.2.1"}/"ocaml\1" {= "4.14.2"}/' |
  sed 's/"ocaml-compiler-libs" {= "v0.17.0"}/"ocaml-compiler-libs" {= "v0.12.4"}/' |
  sed 's/"ocaml-config" {= "3"}/"ocaml-config" {= "2"}/' |
  sed 's/"rope" {= "0.6.3"}/"rope" {= "0.6.2"}/' |
  sed 's/"sqlite3" {= "5.2.0"}/"sqlite3" {= "5.2.0"}\n  "stdcompat" {= "19"}/' \
    > scripts/ci/ocaml4_job/opam/virtual/octez-deps.opam.locked

pwd

DOCKERFILE=${1:-images/packages/debian-deps-build.Dockerfile}

docker build \
  --network host \
  -f "$DOCKERFILE" \
  --build-arg IMAGE="debian:bookworm" \
  -t "${GCP_REGISTRY}/$CI_PROJECT_NAMESPACE/tezos/oc.build-deps-ocaml4" \
  --push \
  scripts/ci/ocaml4_job/
