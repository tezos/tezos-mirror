# Docker build for the testnet experiment tools

This directory is a standalone fork of the root Octez Docker distribution
build (`build.Dockerfile`, `Dockerfile` and `scripts/create_docker_image.sh`
at the repository root), trimmed down to build the images used by the testnet
experiment tools with plain `docker build`.

Being a fork, it does not automatically follow changes to the root build.

## Warning: drift from the root build (#8357)

Issue #8357 removed the internal rust-toolchain image from the root build.
The changes below have NOT been ported here and must be taken into account
when updating or re-syncing this build:

- !22409 merged the `RUST_TOOLCHAIN_IMAGE_NAME` + `RUST_TOOLCHAIN_IMAGE_TAG`
  pair into a single full reference, `RUST_TOOLCHAIN_IMAGE`; the root
  `create_docker_image.sh` now takes `--rust-toolchain-image`.
- !22410 builds the EVM artifacts directly `FROM debian-rust:trixie` and
  requires `--rust-toolchain-image` for with-EVM targets. It also dropped the
  `rust_toolchain_image_name` export from `scripts/version.sh`, which the
  `create_docker_image.sh` here sources.
- !22430 deleted the internal rust-toolchain image entirely
  (`images/rust-toolchain/` and its `oc.docker:rust-toolchain` builder job):
  `<registry>/tezos/tezos/rust-toolchain:*` images are no longer published
  and existing tags will be garbage-collected by the registry retention
  policy.

Impact on this directory: the `RUST_TOOLCHAIN_IMAGE_NAME` /
`RUST_TOOLCHAIN_IMAGE_TAG` plumbing (positional arguments `$8`/`$9` of
`create_docker_image.sh`, forwarded to `build.Dockerfile` ARGs) is dead code —
the ARGs are declared but consumed by no stage (this build has no
`layer2-builder` stage) — and any value passed would now name an image that no
longer exists. Drop it when syncing.

Unrelated to #8357 but relevant to a wholesale re-sync: the root build has
since moved from plain `docker build` to `docker buildx bake`
(`docker-bake.hcl`), with the runtime and build-dependencies images passed as
explicit full references.
