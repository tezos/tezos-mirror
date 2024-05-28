#!/bin/sh

cat << EOT
This script is deprecated.

To update dependencies, you have to:

- in tezos/tezos:
  - update version constraints in manifest/ and run: make -C manifest
  - update full_opam_repository_tag in: scripts/version.sh
  - update the lock file in: opam/virtual/octez-deps.opam.locked
    (for instance using: scripts/update_opam_lock.sh)

- in tezos/opam-repository:
  - update opam_repository_commit_hash in: scripts/version.sh
    (to match full_opam_repository_tag from scripts/version.sh from tezos/tezos)
  - update the opam lock file: octez-deps.opam.locked
    (copy opam/virtual/octez-deps.opam.locked from tezos/tezos)

More information in the documentation:
http://tezos.gitlab.io/developer/contributing-adding-a-new-opam-dependency.html
EOT

exit 1
