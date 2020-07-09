.. _proposal_testing:

How to test a protocol proposal
===============================

In this tutorial we show how to test a newly developed protocol and,
in particular, how to test the migration process from its
predecessor. We also provide a short guide on how to read the
migration code.

We start by describing the branch that contains the protocol proposal
that is under development.

The branch ``proto-proposal``
-----------------------------

The current proposal from Nomadic is developed in the branch
``proto-proposal``, which contains both the new protocol and its
migration code from the current active protocol. The current protocol
proposal under development is referred to as the "Alpha protocol".

Since the commits for migration code are only used once, we keep them
clearly separated by marking them with the tag ``Migration:``. The
first step when developing a new protocol is to revert the migration
commits from the previous protocol. The rest of the comits are used as
a base for current proposals.

The commits in ``proto-proposal`` are mostly confined to the directory
``proto_alpha``. Any change outside this directory is to adapt the
client, the daemons or the test frameworks to the new protocol.

Conversely, the commits in the ``master`` branch should never touch
the directory ``proto_alpha/lib_protocol``.

We next describe how to run unit tests and how to activate the Alpha
protocol in a sandboxed node.

Unit tests and sandbox
----------------------

The first step for tweaking and testing the current proposal is to
checkout the latest code and run unit tests::

  git checkout proto-proposal
  make
  make test

The sandbox script allows one to activate the Alpha protocol---located
in the directory ``proto_alpha``---by using the command
``tezos-activate-alpha``. We can use the usual commands to run a node,
client and daemons for further manual testing::

  ./src/bin_node/tezos-sandboxed-node.sh 1 --connections 0 &
  eval `./src/bin_client/tezos-init-sandboxed-client.sh 1`
  tezos-activate-alpha

After activating the Alpha protocol we can use the usual RPCs to bake
with the client and make the chain progress (see :ref:`sandboxed-mode`)::

  tezos-client bake for bootstrap1

In the following section we provide detailed indications on how to
write migration code, and how to run and bench a migration on a real
context from `Mainnet`.


Test migration on Mainnet
-------------------------

The most delicate part of developing a new protocol is migrating the
context of its predecessor. Migration code often has to convert large
data structures, therefore it is important to bench its running time
and the size of the context produced. For these reasons it is
imperative to run the migration on a real context from Mainnet, bench
it and manually inspect the content of the storage. This process is
somewhat manual but we have tried to automate most of it with a few
scripts.

Snapshot the Alpha protocol
~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to test the Alpha protocol ``proto_alpha`` contained in
``proto_proposal``, we first need to convert the protocol to a format
suitable for Mainnet, a procedure usually referred to as `to snapshot
the Alpha protocol`. This procedure consists mainly of three steps:

- specify the version and name of the current protocol (in
  ``raw_context.ml``),
- compute the protocol's hash (in ``TEZOS_PROTOCOL``), and
- rename a bunch of protocol tags (several places in the code base).

These steps are performed by the script ``scripts/snapshot_alpha.sh``,
which receives a parameter with the name of the Alpha protocol. This
name parameter follows the convention
``<tag_starting_with_version_letter>_<version_number>``. For
historical reasons version ``004`` corresponds to letter ``a``. In the
remainder we will illustrate the migration process through an example
where we migrate to the version ``007``, which corresponds to the
letter ``d``. A valid name for the Alpha protocol in our example would
be ``d_007``. Invoking the script::

  ./scripts/snapshot_alpha.sh d_007

snapshots the Alpha protocol and creates a new directory
``src/proto_007_<hash>`` where the ``<hash>`` computed by the script
coincides with the one in the file ``TEZOS_PROTOCOL``. The new folder
contains the Alpha protocol in a format that can be linked in the node
and client in the build system.

Link the Alpha protocol in the build system
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Protocol ``proto_007_<hash>`` can now be linked in the node and client
in the build system. Note that linking the protocol is not mandatory;
we can always inject a protocol that compiles in a node and link it
dynamically on the fly. However, linking the protocol in the client
enables the use of the commands that may be present in the folder
``lib_client`` of the protocol, if any. Otherwise, only the commands
accessible through the RPCs would be available. Except for some
specific scenarios in which the commands accessible through the RPCs
are enough, it is always convenient to link the protocol in the build
system. In our example, this can be done by invoking::

  ./scripts/link_protocol.sh src/proto_007_*

User-activated upgrade
----------------------

The current Alpha protocol supports self-amending through the voting
procedure of Tezos. However, such procedure can take up to three
months, and we would rather test our migration without waiting such a
long period of time. Besides the amendments driven by the protocol,
Tezos also supports `user-activated` upgrades that are triggered by
the shell. These upgrades are useful in case of emergency bug fixes,
and have been used to migrate from 001 to 002 and from 002 to 003.

The user-activated upgrades can be used to trigger an amendment to any
version of the protocol. In the next section we show how to use this
mechanism in the sandbox, to test the migration from an empty context,
and also in a realistic migration scenario where the context is
imported from a Mainnet snapshot.

User-activated upgrade in the sandbox
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The sandbox starts from an empty context, which can be useful to do
some preliminary migration testing. Instead of activating directly the
version of the Alpha protocol under development, we could activate its
predecessor, and then set the activation of the Alpha protocol at a
given level. For instance, by invoking::

  ./scripts/user_activated_upgrade.sh src/proto_007_* 3

we set a user-activated upgrade that will activate the Alpha protocol
in our example when the blockchain reaches level three. Now we
can start the sandbox as usual and use the client commands to bake
three blocks, after which the Alpha protocol ``proto_007_<hash>`` will
become active.


Import a Mainnet context from a snapshot
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to test the migration from a real Mainnet context we first
need to import the context from a snapshot (see :ref:`snapshots`). We
also need to generate a node identity, which we will keep together
with the imported context in a folder with an ``-orig`` suffix. For
instance, the following commands import a context from the snapshot
``~/488274.roll`` into the folder ``~/488274-node-orig``, and generate
an identity in the same folder::

  ./tezos-node snapshot import ~/488274.roll --data-dir ~/488274-node-orig
  ./tezos-node identity generate --data-dir ~/488274-node-orig

Now, each time we want to test the migration, we copy the contents of
``-orig`` folder into a fresh folder where the migration will be
performed. In our example, we do this with the fresh folder
``~/tezos-node-test`` by invoking::

  cp -r ~/488274-node-orig ~/tezos-node-test

At this stage, it is important to note the level at which the snapshot
was taken, in our example ``488274``.


Yes-node and yes-wallet
~~~~~~~~~~~~~~~~~~~~~~~

Once we start a node from a Mainnet context, how can we make the chain
progress? We don't have the rights to produce blocks and we don't know
the private keys of existing bakers at this point!

The solution is to patch the code to make a `yes-node` that forges and
verifies fake signatures.  This can be achieved with a small patch in
``lib_crypto/signature.ml`` that replaces signatures with a
concatenation of public key and message, such that this fake signature
is still unique for each key and message.

Once we have a yes-node we need to create a `yes-wallet`, which is a
special wallet where secret keys actually encode the same bytes as
their corresponding public keys. By adding to the yes-wallet the
existing accounts of large bakers in Mainnet, e.g. the foundation, we
would have enough rights to bake blocks at will. In our exmaple, we
can generate such a wallet with::

  ./scripts/user_activated_upgrade.sh src/proto_007_* 488277

Triggering the migration is then a matter of::

  ./tezos-client -d yes-wallet bake for foundation1 --minimal-timestamp
  or
  ./tezos-baker-007-* -d yes-wallet run with local node ~/488274-node foundation1

Since we set the user-activated upgrade at level three, we should see
the change of protocol after baking three blocks.

Note: to test the migration again, remember to remove spurious files
such as ``blocks`` or ``wallet_lock`` in the yes-wallet directory and
to recopy the contents of the ``-orig`` folder into the folder where
the test will be performed.

Wrap it up
~~~~~~~~~~

Here's a full example of how to modify and test ``proto-007``::

  git checkout proto-proposal

Change line 47 of ``src/proto_alpha/lib_protocol/init_storage.ml`` as follows::

  | Carthage_006 ->
      Logging.log_notice "\nSTITCHING!\n" ;

Then::

  $ git commit -am 'My awesome feature'
  $ ./scripts/snapshot_alpha.sh d_007
  $ ./scripts/link_protocol.sh src/proto_007_*
  $ ./scripts/user_activated_upgrade.sh src/proto_007_* 488277

  #patch yes-node
  $ patch -p1 < ./scripts/yes-node.patch

  #create yes_wallet directory
  $ dune exec ./script/yes-wallet/yes_wallet.exe ~/yes-wallet

  # import Mainnet context and keep imported clean of modification
  $ ./tezos-node snapshot import ~/488274.roll --data-dir ~/488274-node-orig
  $ ./tezos-node identity generate --data-dir ~/488274-node-orig
  $ cp -r ~/488274-node-orig ~/tezos-node-test

  $ make

  # Run the node
  $ ./tezos-node run --connections 0 --data-dir ~/tezos-node-test --rpc-addr localhost &

  $ curl -s localhost:8732/chains/main/blocks/head/metadata | jq '.level.level, .protocol, .next_protocol'
  488274
  "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
  "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"

  # baking a regular 006 block should be quick
  $ time ./tezos-client -d yes-wallet bake for foundation1 --minimal-timestamp

  # baking the migration block to 007 takes longer and you should see the
  # STITCHING! message in the logs
  $ time ./tezos-client -d yes-wallet bake for foundation1 --minimal-timestamp

  # the context resulted from the application of
  # block 488276 is understood by 007
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
the RPCs ``context/raw/json`` and ``context/raw/bytes``. The former
RPC displays the json value relative to a key of the context, using
its json format. This is possible thanks to the storage functors of
Tezos, which are used to register every piece of storage in a node and
are aware of the json structure of the data. The latter RPC is more
low level and simply returns the bytes corresponding to a key. Both
RPCs support the option `depth` to control how much of the subtree of
the key should be displayed.

For example, if we use ``context/raw/json`` to inspect the size of the
current listings, which informs of how many rolls are allowed to vote
in the current period, we get::

  $ curl -s localhost:8732/chains/main/blocks/head/context/raw/json/votes/listings_size
  56639

On the other hand, if instead we use ``context/raw/bytes`` to inspect
the data corresponding to the same key, we obtain a string of bytes in
hexadecimal format::

  $ curl -s localhost:8732/chains/main/blocks/head/context/raw/bytes/votes/listings_size
  "0000dd3f"

This string of bytes can be converted using the OCaml toplevel to
obtain the same value retrieved before::

  utop # let h = 0x0000dd3f ;;
  val h : int = 56639


In our migration example above, we can inspect the json output of a
specific contract::

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

The ``raw/json`` interface conveniently hides the disk representation
of data and keys. Notice how the hashes of public keys are not stored
as is, but instead they are encripted using the more efficient base58
format.

In this case, in order to inspect the low level representation in
bytes, which we would often need to, we have to convert hashes of
public keys using ``utop`` and the functions ``of_b58check`` and
``to_b58check`` of module ``Contratc_repr``::

  # let's borrow some code from the protocol tests
  $ dune utop src/proto_007_*/lib_protocol/test/

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

On the other hand, we could have inspected the data corresponding to
the same key above with ``raw/bytes``, as we do below::

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

Observe that while the value in json format above shows a ``big_map``
field that is empty (i.e. ``"big_map": [],``), the low-level
representation of the same value reveals that the field containing
such an empty ``big_map`` is not stored at all.


Anatomy of migration code
-------------------------

The migration code is triggered in
``init_storage.ml:prepare_first_block``, so that method is the entry
point to start reading it. Notice that constants are migrated in
``raw_context.ml:prepare_first_block``, which takes a ``Context.t``
and returns a ``Raw_context.t`` containing the new
constants. Migrating other data can usually be done by manipulating
the ``Raw_context.t``, and such code should be placed in the match
case ``Alpha_previous`` of ``init_storage.ml:prepare_first_block``.

Conversion of data structures from the previous protocol are typically
found in ``storage.ml,i``, which may involve the functors in
``storage_functors.ml,i``.  Each migration is very custom, but there
are two recurring schemas that emerged over time.

For high-level changes, the interface offered by the
``storage_functors`` is usually expressive enough. The migration would
copy the code to read the data structures in the previous version and
simply rename it by adding a suffix with the previous version number
(in our example above where we are migrating to version ``007``, the
indentifiers in the old code would be renamed by appending the suffix
``_006`` to them). The values are then written using the code for the
data structures of the current protocol, thus performing the
migration. The last step in the migration would be to manually remove
any remaining code with a suffix corresponding to the previous version
(``_006`` in our example).

Some migrations may requires to break the interface offered by the
``storage_functors``, and to modify the file ``raw_context.mli``
directly. In this case we usually `copy` the data to a temporary path,
perform the conversion, and then `recursively remove` the temporary
path.
