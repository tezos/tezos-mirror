Get_contracts script
====================

The purpose of this tool is to extract all smart contracts (their
addresses, storage and code) for a Tezos node's context. It is
somewhat dependent on the internals of the blockchain's protocol,
hence we need a separate binary for each protocol. Still, most of the
logic is contained in the generic `Get_contracts` module, which is
separated from the protocol by a compatibility layer, whose signature
is given by the `Sigs.PROTOCOL` module type. Each of the protocol-specific
modules defines a compatibility layer for that particular protocol and
generates an executable.

The script can be run using dune:

```
$ dune exec devtools/get_contracts/get_contracts_alpha.exe -- /path/to/the/context /path/to/output
```

`NOTE`: the second argument defaults to the current directory.
  
This script was originally developed as a protocol-specific binary on
the branch: https://gitlab.com/nomadic-labs/tezos/-/tree/vbot@get_contracts .
However, it was then decided that it should rather be stored in `devtools`
directory, so it was copied in its then-current form the to a new branch,
thus losing previous development history.

Whenever a new protocol is snapshotted, this script requires the
following adjustments:
  1. `get_contracts_alpha.ml` should be copied with an appropriate name
  2. `dune` should be updated with a new executable definition
  
At the same time it's possible to delete executables for old protocol
versions, that won't be needed anymore, to ease the maintenance burden.
**All the above steps are currently performed by the Manifest**
(`src/manifest/main.ml`). They can also be invoked manually by calling
`make -C manifest`.
