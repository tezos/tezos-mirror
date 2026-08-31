This directory contains various utility scripts related to the project lifecycle.

# Overview

In particular, this includes scripts for:
* installing the project environment (`install_*.sh`)
* setting up environment variables for building or using Tezos (`env.sh`)
* generating the Docker image used by the CI (`create_docker_image.sh`, `ci/`)
  or used by users and developers (`docker/`)
* dune files instrumentation for computing test coverage (`instrument_dune_bisect.sh`)
* maintaining a custom opam repository with all Tezos dependencies
 (`update_opam_repo.sh`, `opam-*.sh`)
* generate base58 prefixes (`b58_prefix/`)
* generate configuration code (in OCaml) to bootstrap a new network (`gen-genesis/`)
* preparing the environment for migration tests (`prepare_migration_test.ml`)
* releasing static Octez binaries (`release/`)
* snapshotting protocols, linking them to the build system (`snapshot_alpha.sh`, `link_protocol.sh`, `snapshot_alpha_and_link.sh`)
* getting baker version stats from a range of blocks (`baker-version-stats.sh`)
* printing a dependency tree based on ocamldep output (`ocamldot.py`)
* creating a yes-wallet, to be used in conjunction with a yes-node (`yes-wallet/` and `yes-node.patch`)
* setting user-activated upgrades (`user_activated_upgrade.sh`)
* generating commitments (initial accounts) for test networks (`create_genesis/create_genesis_info.py`)
* patch the protocols to add profiling (`patch-profiler-proto.sh`)
* estimating when the next protocol activation will occur, in the best case and
  under block-time drift assumptions (`next_protocol_activation.py`)
* listing the bakers assigned to rounds [0; n] of a block with their name and the
  Octez version each currently runs (`baker_round_versions.py`)
* measuring the per-round block distribution over a recent window (from tzkt) and
  emitting a `--drift` list for `next_protocol_activation.py` (`round_distribution.py`)
* estimating the next protocol activation using the round distribution measured over
  the last N blocks (N = blocks-to-activation), combining the two tools above
  (`activation_empirical_drift.py`)

This directory also includes an example docker-compose file to run a
node with a baker and an accuser (`docker/docker-compose-generic.yml`)
and a minimalistic Michelson REPL built on top of `octez-client`
(`michelson_repl.sh`).

## API
<!-- For each script where this is possible, indicate how to obtain usage info
  (e.g. invoke with no args; or link to doc page where the tool is explained), and
  maybe an example of use. -->

Information about using some of the scripts above can be found as follows:

* `instrument_dune_bisect.sh`: See <https://tezos.gitlab.io/developer/testing.html>.
* `update_opam_repo.sh`: see <https://tezos.gitlab.io/developer/contributing-adding-a-new-opam-dependency.html>
* `version.sh`: see <https://tezos.gitlab.io/introduction/howtoget.html> and <https://tezos.gitlab.io/developer/contributing-adding-a-new-opam-dependency.html>
* `yes-wallet/`: see <http://tezos.gitlab.io/developer/proposal_testing.html>
* `user_activated_upgrade.sh`: see <http://tezos.gitlab.io/developer/proposal_testing.html>
* `next_protocol_activation.py`: run `python3 scripts/next_protocol_activation.py --help`;
  it queries a node (default `http://localhost:8732`, read-only) and prints the best-case
  and drift-adjusted activation date/time.
* `baker_round_versions.py`: run `python3 scripts/baker_round_versions.py --help`;
  it queries a node (read-only) for the bakers assigned to rounds [0; n] of a block and
  annotates each with its name and Octez version from tzkt (network auto-detected).
* `round_distribution.py`: run `python3 scripts/round_distribution.py --help`; it measures
  the per-round block distribution over a window (default last two weeks) from tzkt and
  prints a `--drift` list for `next_protocol_activation.py`. Network auto-detected from a
  node's chain id; `--list-only` is pipeable.
* `activation_empirical_drift.py`: run `python3 scripts/activation_empirical_drift.py --help`;
  it combines the two scripts above — measuring the round distribution over the last
  blocks-to-activation blocks and feeding it as the drift for the activation estimate.
  By default it reports both the head-anchored and previous-activation-anchored empirical
  results (`--anchor` restricts to one).

## Profiler patches for the protocols

If the profiler patches are conflicting for some reason, the simplest is
basically to regenerate them (assuming the conflicting branch is based on
`master`, otherwise replace it with your branches' upstream):

```
scripts/update_profiler_patch.sh
```

and follow its guidance.
