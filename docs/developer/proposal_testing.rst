.. _proposal_testing:

How to test a protocol proposal
===============================

In this tutorial we show how to test a newly developed protocol and in
particular the migration from its predecessor.
At the end there is also a short guide on how to start reading the
migration code.

Branches
--------

The current proposal from Nomadic is developed in the branch
``proto-proposal``, that contains both the new protocol and its
migration code from 004.
Previous protocols can be found in the branches:

- `proto-003` and `proto-003-migration`
- `proto-004` and `proto-004-migration`

We keep the commits for migration code separated from the rest because
they are only used once, while the commits for the protocol are used as a
base for future protocols.
For example `proto-003-migration` contains a few migration commits on
top of `proto-003`, but these commits are of no use for `proto-004` which
is based directly on `proto-003`.
In the same vein `proto-proposal` (the future proto-005) is based
directly on `proto-004`.

The branches `*-migration` are those generating the `proto-00?-<hash>`
that are present in `mainnet`.
We'll see in a moment how they can be converted.

In principle all the commits present in these branches are mostly
confined to the directory `proto_alpha`.
Any change outside is just to adapt test frameworks to the new protocol.


Unit tests and sandbox
----------------------

The first step for tweaking and testing the current proposal is to
checkout the latest code, experiment and run unit tests::

  git checkout proto-proposal
  make
  make test

The sandbox script automatically activates the lastest `proto_alpha`
so we can use the usual commands to run a node, client and daemons for
further manual testing::

  ./src/bin_node/tezos-sandboxed-node.sh 1 --connections 0 &
  eval `./src/bin_client/tezos-init-sandboxed-client.sh 1`
  tezos-activate-alpha

We can now do some RPCs and bake as usual with the client to make the
chain advance (see :ref:`sandboxed-mode`)::

  tezos-client bake for bootstrap1


Test migration on Mainnet
-------------------------

The most delicate part of developing a new protocol is migrating the
context of its predecessor.
Migration code often has to convert large data structures, therefore
it is important to bench its running time and the size of the context
produced.
For these reasons it is imperative to run the migration on a real
context from Mainnet, bench it and manually inspect the content of
the storage.
This process is somewhat manual but we have tried to automate most of
it with a couple of scripts `snapshot_alpha` and `activate_protocol`.

Snapshot the rolling proto_alpha
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to test a `proto_alpha` from `proto_proposal`, we first need
to convert it in a format suitable for `mainnet`.
This process consists mainly of four steps:

- specify the version and name of the current protocol (in `raw_context.ml`)
- specify who is the predecessor (in `raw_context.ml`, `init_storage.ml`)
- compute the hash (in `TEZOS_PROTOCOL`)
- do a bunch of renamings

These steps are run by `scripts/snapshot_alpha.sh`::

  ./scripts/snapshot_alpha.sh b_005 from athens_004

We now have a new directory `src/proto_005_<hash>` that is ready for `mainnet`::

  git checkout mainnet
  git commit src/proto_005_* -m 'Import protocol 005'


Import a Mainnet context from a snapshot
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The simplest way to get a Mainnet context is to import it from a
snapshot (see :ref:`snapshots`).
We also generate a node identity and keep everything in a directoty
`-orig` that we will use to make single-use node directories for our
tests::

  ./tezos-node snapshot import ~/488274.roll --data-dir ~/488274-node-orig
  ./tezos-node identity generate --data-dir ~/488274-node-orig
  cp -r ~/488274-node-orig ~/tezos-node-test

Make sure you note the level that you took the snapshot at, in this
case 488274.

Activate in the build system
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Once `proto_005_<hash>` is in the mainnet branch together with the other
protocols, we need to link it in a couple of places in the build
system, in particular in the node and in the client.
Note that this is not mandatory, we can always inject a protocol in a
node that will compile and dynlink it on the fly.
If we don't link the protocol in the client however we will not be
able to use any of the specific commands that may be present in the
`lib_client` of the protocol, if any.
We would have access to RPCs though, which is often enough.
Except for specific scenarios, it's convenient to link both.

User-activated update
~~~~~~~~~~~~~~~~~~~~~

The current protocol alpha supports self-amending through a voting
procedure, however we'd like to test our migration without waiting 3
months.
Other than amendments driven by the protocol, Tezos supports
`user-activated` amendments that are triggered by the shell.
These updates are useful in case of emergency bug fixes and have been
used to go from 001 to 002 and from 002 to 003.

The same mechanism can be used to trigger an amendment to protocol 005
at a specific level, say 3 levels after our current context level.

Yes-node and yes-wallet
~~~~~~~~~~~~~~~~~~~~~~~

Once we start a node from a Mainnet context however, how can the chain
progress?
Indeed we don't have the rights to produce blocks and we don't know
the private keys of existing bakers!

The solution is to patch the current `mainnet` code to make a
`yes-node` that forges fake signatures.
This can be achieved with a small patch in `lib_crypto/Signature.ml`
which replaces signatures with a concatenation of public key and
message, so that they are still unique for each key and
message.

Once we have a yes-node we need to create a yes-wallet, which is a
special wallet where secret keys actually encode the same bytes as
their corresponding public keys.
If we add to the yes-wallet the existing accounts of large bakers in
Mainnet, e.g. the foundation, we should have enough rights to bake
blocks at will.
The script `scripts/yes-wallet.ml` can be used to generate such a
wallet.

Then it's a matter of::

  ./tezos-client -d yes-wallet bake for foundation1 --minimal-timestamp
  or
  ./tezos-baker-005-* -d yes-wallet run with local node ~/488274-node foundation1

Note: remember to remove spurious files such as ``blocks`` or ``wallet_lock``
from one test to the other.


Wrap it up
~~~~~~~~~~

Build system, user activated update and yes-node are taken care of by
`scripts/activate_protocol.sh`.

Here's a full example of how to modify and test proto-005::

  git checkout proto-proposal

Change `src/proto_alpha/lib_protocol/init_storage.ml` like so at line 47::

  | Alpha_previous ->
      Logging.log_notice "\nSTITCHING!\n" ;

Then::

  $ git commit -am 'My awesome feature'
  $ rm -rf src/proto_00* && ./scripts/snapshot_alpha.sh b_005 from athens_004
  $ git checkout mainnet

  # cherry-pick the activate_protocol.sh script in proto-proposal
  $ git log --oneline proto-proposal | grep yes-node | head -1
  dddf3e48a Scripts: add yes-node to activate_protocol.sh
  $ git cherry-pick dddf3e48a

  # activate using 488276 for the user-activated update
  $ ./scripts/activate_protocol.sh src/proto_005_*
  Link in the Node? (no if you want to test injection) (Y/n)
  User-activated update? (Y/n)
  At what level? (e.g. 3 for sandbox): 488276

  $ make

  $ ./tezos-node run --connections 0 --data-dir ~/tezos-node-test --rpc-addr localhost &

  $ curl -s localhost:8732/chains/main/blocks/head/metadata | jq '.level.level, .protocol, .next_protocol'
  488274
  "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
  "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"

  # baking a regular 004 block should be quick
  $ time ./tezos-client -d yes-wallet bake for foundation1 --minimal-timestamp

  # baking the migration block to 005 takes longer
  $ time ./tezos-client -d yes-wallet bake for foundation1 --minimal-timestamp
  Jun 30 16:26:37 - 005-PscqRYyw:
  Jun 30 16:26:37 - 005-PscqRYyw: STITCHING!
  Jun 30 16:26:37 - 005-PscqRYyw:
  [...]

  # the context resulted from the application of
  # block 488276 is understood by 005
  $ curl -s localhost:8732/chains/main/blocks/head/metadata | jq '.level.level, .protocol, .next_protocol'
  488276
  "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
  "PscqRYywd243M2eZspXZEJGsRmNchp4ZKfKmoyEZTRHeLQvVGjp"

  # kill the node, a little cleanup and we are ready for another test
  $ fg
  ./tezos-node run --connections 0 --data-dir ~/check/tezos-heavy/488274-node --rpc-addr localhost
  ^C
  $ rm -rf ~/tezos-node-test && cp -r ~/488274-node-orig ~/tezos-node-test && rm -f yes-wallet/{wallet_lock,blocks}


Tips and tricks
---------------

Migrating a context mostly concerns editing existing data structures.
For this reason it is important to inspect the resulting context with
the RPCs `context/raw/json` and `context/raw/bytes`.
The former displays the json value relative to a key of the context, thanks
to functions that are registered by the storage functors and that have
knowledge of the format of the data.
The latter is more low level and simply returns the bytes
corresponding to a key.
They both support the option `depth` to control how much of the
subtree of the key should be displayed.

For example if we inspect the size of the current listings, that is
how many rolls are allowed to vote in the current period, we get::

  $ curl -s localhost:8732/chains/main/blocks/head/context/raw/json/votes/listings_size
  56639

On the other hand, in bytes we get in hexadecimal format::

  $ curl -s localhost:8732/chains/main/blocks/head/context/raw/bytes/votes/listings_size
  "0000dd3f"

Which can be converted simply using the OCaml toplevel to obtain the same value::

  utop # let h = 0x0000dd3f ;;
  val h : int = 56639


Following the more complex example above, we can inspect the json
output of a specific contract::

  $ curl -s localhost:8732/chains/main/blocks/head/context/raw/json/contracts/index/tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5 | jq .
  {
    "balance": "2913645407940",
    "big_map": [],
    "change": "2705745048",
    "counter": "0",
    "delegate": "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5",
    "delegate_desactivation": 125,
    "delegated": [],
    "frozen_balance": [],
    "manager": "p2pk66n1NmhPDEkcf9sXEKe9kBoTwBoTYxke1hx16aTRVq8MoXuwNqo",
    "roll_list": 50696,
    "spendable": true
  }

The `raw/json` interface is conveniently hiding the disk
representation of data and keys.
For example in the contract index, public key hashes are not stored as is
but using a more efficient format.

If we want to inspect the low level representation in bytes, and we
often need to, we first need to convert the public key hash of the
account in its disk format.
We can use `utop` and a couple of functions to do that::

  # let's borrow some code from the protocol tests
  $ dune utop src/proto_005_*/lib_protocol/test/

  # open Tezos_protocol_alpha.Protocol ;;

  # let b58check_to_path c =
  Contract_repr.of_b58check c |> fun (Ok c) ->
  Contract_repr.Index.to_path c [] |>
  String.concat "/"
  ;;
  # b58check_to_path "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5" ;;
  ff/18/cc/02/32/fc/0002ab07ab920a19a555c8b8d93070d5a21dd1ff33fe

  # let path_to_b58check p =
  String.split_on_char '/' p |>
  Contract_repr.Index.of_path |> fun (Some c) ->
  Contract_repr.to_b58check c
  ;;
  # path_to_b58check "ff/18/cc/02/32/fc/0002ab07ab920a19a555c8b8d93070d5a21dd1ff33fe"  ;;
  "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5"

Now we can use the path with the `raw/bytes` RPC::

  $ curl -s localhost:8732/chains/main/blocks/head/context/raw/bytes/contracts/index/ff/18/cc/02/32/fc/0002ab07ab920a19a555c8b8d93070d5a21dd1ff33fe | jq .
  {
    "balance": "c4ddb296e654",
    "change": "98c9998a0a",
    "counter": "00",
    "delegate": "02ab07ab920a19a555c8b8d93070d5a21dd1ff33fe",
    "delegate_desactivation": "0000007d",
    "delegated": {
      "15": {
        "bb": {
          "9a": {
            "84": {
              "b5": {
                "e3501428362c63adb5a4d12960e7ce": "696e69746564"
              }
            }
          }
        }
      },
      ...
    },
    "frozen_balance": {
      "114": {
        "deposits": "80e0f09f9b0a",
        "fees": "93bb48",
        "rewards": "809ee9b228"
      },
      ...
    },
    "manager": "0102032249732e424adfaf6c6efa34593c714720c15490cdb332f2ac84ef463784ff4e",
    "roll_list": "0000c608",
    "spendable": "696e69746564"
  }

In this case we can see that while the json shows a `big_map` field
that is empty, the bytes RPC reveals that the field is not stored at
all.


Anatomy of migration code
-------------------------

The migration code is triggered in
``init_storage.ml:prepare_first_block``, so that's the entry point to
start reading it.
If there is need to migrate constants, this is done in
``raw_context.ml:prepare_first_block`` which takes a ``Context.t`` and
returns a ``Raw_context.t`` that contains the new constants.
The rest of the migration usually can be done manipulating a
``Raw_context.t`` and should be done in the match case
``Alpha_previous`` of ``init_storage.ml:prepare_first_block``.

When there is need to convert data structures from the
previous protocol, these changes are typically found in
``storage.ml,i`` and may involve the functors in
``storage_functors.ml,i``.
Each migration is very custom, but there are two recurring schemas
that emerged over time.

For relatively high level changes, the interface offered by the
`storage_functors` is expressive enough.
In this case, the code to read the old data structure is copied from
the previous protocol and renamed with a suffix `_004`.
The values are then written using the data structure of the current
protocol, thus performing the migration.
Lastly we remove the `_004` data and we are done.

Some migration code requires to break the interface of
`storage_functors` and to use directly `raw_context.mli`.
In this case we usually `copy` the data to a temporary path, perform
the conversion and then `remove_rec` the temporary path.
