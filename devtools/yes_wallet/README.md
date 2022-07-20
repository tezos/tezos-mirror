Yes_wallet script
=================

The purpose of this tool is to extract baker addresses from a node's
context and generate tezos-client's wallet containing these addresses.
In combination with the yes-node patch it can be used to perform a test
protocol migration (see the relevant documentation page for more details)
or to reproduce contexts where trouble occurred in the past in order to
debug them.

The script can be run using dune:

```
$ dune exec devtools/yes_wallet/yes_wallet.exe -- create from context /path/to/context in /path/to/wallet
```

The script will automatically detect the protocol of the given context
and use the appropriate module to extract the data.

Whenever a new protocol is snapshotted, this script requires the
following adjustments, **which are currently being automatically
performed by the Manifest** (`src/manifest/main.ml`):
  1. `get_delegates_alpha.ml` should be copied with an appropriate name
  2. `dune` should be updated with a new executable definition
  3. delete modules supporting frozen protocol versions to ease the maintenance burden.

The Manifest can also be invoked manually by calling `make -C manifest`.

Support for older protocols (down to Ithaca) can be restored relatively
easily using git.

For instance to revive support for Ithaca:

* `git checkout <commit-or-tag> -- devtools/yes_wallet/get_delegates_012_PsIthaca.ml`
* modify the `dune` file to import `tezos-protocol-012-Psithaca`
* `dune build`

The `<commit-or-tag>` refers to some git revision, where the support
was still maintained (given protocol was active).

After that, the support for the given protocol should be restored.
