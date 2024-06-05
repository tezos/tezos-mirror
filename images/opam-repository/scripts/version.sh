#!/bin/sh

# Base docker image
export alpine_version='3.19'

# Installed via apk cargo in runtime-prebuild-dependencies.Dockerfile
export cargo_version='1.76.0'

# Installed via apk rust in runtime-prebuild-dependencies.Dockerfile
export rust_version='1.76.0'

# Installed via apk rust in runtime-prebuild-dependencies.Dockerfile
export opam_version='2.1.5'

# Which opam repository commit to rely on for package definitions
# Must match the value in tezos/tezos scripts/version.sh
export opam_repository_commit_hash='0bdad08fb7e8f889d600aa06619fdb88cd179258'

# Installed via opam in runtime-prebuild-dependencies.Dockerfile
export ocaml_version='4.14.1'

# Installed via apk python3-dev in runtime-build-test-dependencies.Dockerfile
export python_version='3.11.9'

# Installed via apk in runtime-build-test-dependencies.Dockerfile
export poetry_version='1.7.1'

# SHA-256 hashes of the DAL SRSs, as used in 'scripts/install_dal_trusted_setup.sh' to verify
# integrity of downloaded SRS.
export dal_srsu_g1_sha=c48ce4add1de2a7561108f17bf0c16bc1e93c0bff24bc7da465c24e0b4b2653e
export dal_srsu_g2_sha=e7fbe747ae3648a5b664d8f8bd7c524996f7ed07f3331f905d2e73767d580f7c
