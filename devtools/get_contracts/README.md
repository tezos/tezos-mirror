Get_contracts script
====================

The purpose of this tool is to extract all smart contracts (their
addresses, storage and code) for a Tezos node's context. It is
somewhat dependent on the internals of the blockchain's protocol,
hence we need a separate module for each protocol. Still, most of the
logic is contained in the generic `Get_contracts` module, which is
separated from the protocol by a compatibility layer, whose signature
is given by the `Sigs.PROTOCOL` module type. Each of the protocol-specific
modules defines a compatibility layer for that particular protocol and
generates an executable.

The script can be run using dune:

```shell
dune exec devtools/get_contracts/get_contracts.exe -- /path/to/the/context /path/to/output
```

`NOTE`: the second argument defaults to the current directory.

The protocol version is automatically detected.

If you have no context to import contracts from, you need to import a snapshot
(rolling is enough) with:

```shell
dune exec octez-node -- snapshot import <snapshot-file> --data-dir /path/to/the/context [--block <expected block hash>]
```

If you are trying to import a testnet snapshot, you need to configure your node
with the appropriate network name, e.g. `ghostnet`, with

```shell
dune exec octez-node -- config init --network=ghostnet --data-dir /path/to/the/context
```

This script was originally developed as a protocol-specific binary on
the branch: <https://gitlab.com/nomadic-labs/tezos/-/tree/vbot@get_contracts> .
However, it was then decided that it should rather be stored in `devtools`
directory, so it was copied in its then-current form the to a new branch,
thus losing previous development history.

Whenever a new protocol is snapshotted, this script requires the
following adjustments, **which are currently being automatically performed by
the Manifest** (`manifest/main.ml`, invoke with `make -C manifest`):

  1. `get_contracts_alpha.ml` should be copied with an appropriate name
  2. The two `open`s at the top should be adapted

At the same time it's possible to delete modules for old protocol
versions, that won't be needed anymore, to ease the maintenance burden.
