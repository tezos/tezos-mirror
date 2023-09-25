How to Test a Protocol Proposal
===============================

In this tutorial we show how to test a newly developed protocol and, in
particular, how to test the migration process from its predecessor. We also
provide a short guide on how to read the migration code.

We start by describing the branch that contains the protocol proposal that is
under development.

The ``master`` Branch
---------------------

The current proposal is developed in the branch ``master``, which
contains both the new protocol and its migration code from the current active
protocol. The current protocol proposal under development is referred to as the
*Alpha protocol*.

Since the migration code is only used once, it is a good practice to keep them clearly
separated by marking them with the tag ``Proto/Migration:``.
Note that a first step when developing a new protocol is to revert the migration code from the previous protocol.

We next describe how to run unit tests and how to activate the Alpha protocol in
sandboxed node.

Unit Tests and Sandboxed Mode
-----------------------------

The first step for tweaking and testing the current proposal is to checkout the
latest code and run unit tests::

  $ git checkout master
  $ git pull
  $ make
  $ make test

We can run a node and a client in sandboxed mode by invoking::

  $ ./src/bin_node/octez-sandboxed-node.sh 1 --connections 0 &
  $ eval $(./src/bin_client/octez-init-sandboxed-client.sh 1)

By default, the sandbox starts from the ``genesis`` block at level 0, and the
sandbox's active protocol is the ``Genesis protocol``. Once the sandbox is
started, the Alpha protocol can be activated by invoking the command::

  $ octez-activate-alpha

This command inserts a new block after which the active protocol is the Alpha
protocol. From this point on, making the chain progress is straightforward
because the sandbox contains accounts ``bootstrap1`` to ``bootstrap5`` with
implicit credentials that allow them to bake blocks by using the usual RPCs in
the shell (see :doc:`../user/sandbox`)::

  $ octez-client bake for --minimal-timestamp

Adding New Protocol Tests in OCaml
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Be sure you first read the :doc:`introduction on the testing ecosystem for Tezos <testing>`.
In addition to system tests written with :doc:`Tezt <tezt>`,
unit tests and integration tests for the protocol can be found in :src:`src/proto_alpha/lib_protocol/test`.
It is strongly recommended to write unit tests and integration tests in addition to the
system tests.
To test a new component, create a new file in this directory and add the module in ``main.ml``.
To print the errors of the ``Error`` monad correctly, alcotests must be wrapped into
the function ``tztest`` defined in the module ``Test`` defined at the same level.

Some helpers are available in the module ``Tezos_alpha_test_helpers`` defined in
the subdirectory ``helpers``. For instance, it contains context, operation and
block fixtures that can be used in tests requiring these components.


Testing Migration Code
----------------------

The remainder of the tutorial is organized as follows. Section
`Migration testing`_ provides detailed indications on how to test a
migration, and Section `Wrap up the migration procedure`_ summarizes
these indications by collecting all the steps needed to test the
migration. To conclude, Section `Tips and tricks`_ indicates how to
use the shell to inspect the context, and Section `Anatomy of
migration code`_ contains a primer on how to read and write migration
code.


Migration Testing
-----------------

The most delicate part of migrating to a new protocol is to produce a new
context from the context of its predecessor. The migration code takes care of
producing the new context, which often involves converting large data
structures. Therefore, it is important to bench the migration running time and
the size of the context produced. For these reasons it is imperative to test the
migration on a real context imported from Mainnet, bench it, and manually
inspect the content of the storage. We refer to this procedure as "migration on
a context imported from Mainnet".

However, sometimes we may want to perform some preliminary tests and run the
migration on an empty context that we populate manually. We can do so by running
a node in sandboxed mode, and by activating the predecessor of the Alpha
protocol on the genesis block. We refer to this procedure as "migration on the
sandbox".

This section describes a *migration procedure* in which the developer is
in charge of setting up the migration environment and of manually baking the
blocks that would eventually trigger the migration. For convenience, we have
batched parts of this manual migration procedure by providing scripts that
encompass some of its steps.

We will illustrate the migration procedure through an example where the version
of the Alpha protocol to which we migrate is ``012``.


Checkout Latest Code and Tweak Migration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We start by checking out the latest code for the Alpha protocol::

  $ git checkout master
  $ git pull

Now we could tweak our migration by adding any desired feature. For instance, we
could log the point at which migration takes place by editing the file
``src/proto_alpha/lib_protocol/init_storage.ml``. This can be done by modifying
the match expression of the function ``prepare_first_block`` in the said file to
include the following lines::

  | Hangzhou_011 ->
      Logging.(log Notice "STITCHING!") ;

After making sure that our ``master`` branch contains all the migration
code that we want to test, we need to commit the changes locally::

  $ git commit -am 'My awesome feature'

The next section summarizes how to prepare the migration once we have tweaked
the Alpha protocol.


Prepare the Migration
~~~~~~~~~~~~~~~~~~~~~

Preparing the migration comprises the following steps:

1. snapshot the Alpha protocol, if so wished,
2. link the snapshot Alpha protocol in the build system, if we wished to
   snapshot the Alpha protocol,
3. set ``user-activated upgrade`` that will trigger the migration at a given
   level,
4. patch the shell to obtain a ``yes-node`` that can fake baker signatures, if we
   wish to import the context from Mainnet,
5. compile the project,
6. import a context from Mainnet, if so wished, and
7. create a ``yes-wallet`` that stores fake baker signatures, if we wish to import
   the context from Mainnet.


Steps 1--7 can be batched by invoking the script
``scripts/prepare_migration_test.sh`` in the way we explain
below. Alternatively, each of the steps above can be performed individually by
invoking the corresponding commands/scripts that we detail in the rest of the
section.

Before preparing the migration, we need to choose on which context the migration
will run. When on the sandbox, the steps 4, 5 and 7 above are omitted because
the sandbox starts on an empty context, and the sandbox automatically contains
accounts with implicit credentials that will allow us to bake blocks and make
the chain progress.

When on a context imported from Mainnet, we will use a *snapshot file* (do not
mistake "snapshot a protocol", like in step 1 above, with "snapshot a node",
which results in a snapshot file like in here) that contains the real status of
a Mainnet's node at a particular moment in time. Such a snapshot file can be
downloaded from several sites on the internet (see :doc:`../user/snapshots`).
Such websites store
daily snapshot files from both Mainnet and Testnet, in both ``full`` and
``rolling`` mode (see :doc:`../user/history_modes`). For the purposes of testing
the migration, a snapshot file in ``rolling`` mode is enough. It is important to
use a snapshot file that is recent enough as to contain the predecessor of the
Alpha protocol. It is also important to note down the level at which the
snapshot file was taken, which determines at which level we want to trigger the
migration. The snapshots websites
conveniently indicate the date and the level (the block) at which each
snapshot file was taken.

In our example we will use a snapshot file
``~/snapshot-mainnet.rolling``
which was taken at level ``1617344``.

The next subsections explain each of the individual steps 1--7.


1. Snapshot the Alpha Protocol
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Snapshotting the Alpha protocol is an optional procedure whose objective is to
convert the Alpha protocol to a format that could be injected into Mainnet,
which is done by performing the following three steps:

- specify the version and name of the current protocol in ``raw_context.ml``,
- compute the protocol's hash in ``TEZOS_PROTOCOL``, and
- replace names and protocol hashes in various places in the code base.

If so wished, these three steps can be performed by the script
``scripts/snapshot_alpha.sh``, which receives a parameter with the name of the
Alpha protocol. This name parameter follows the convention
``<tag_starting_with_version_letter>_<version_number>``. A valid name for the Alpha protocol
in our example could be ``d_012`` (we might also have used ``dummy_12``).
We can snapshot the protocol by invoking the following::

  $ ./scripts/snapshot_alpha.sh d_012

The script creates a new directory ``src/proto_012_<short_hash>`` where
``<short_hash>`` is a short hash that coincides with the first eight characters
of the hash computed by the script and written in the file ``TEZOS_PROTOCOL``.

If the Alpha protocol has been snapshot, proceed to Section `2. Link the
snapshot Alpha protocol in the build system`_ below, which details how to link
the snapshot code in the build system. Otherwise proceed directly to Section
`3. Set user-activated upgrade`_.


2. Link the Snapshot Alpha Protocol in the Build System
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If the Alpha protocol was snapshot into
``src/proto_<version_number>_<short_hash>``, this protocol can now be linked in
the build system. Note that linking the protocol is not mandatory; we can always
inject a protocol that compiles in a node and link it dynamically on the
fly. However, linking the protocol in the client enables the use of the commands
that may be present in the folder
``src/proto_<version_number>_<short_hash>/lib_client``, if any. Otherwise, only
the commands accessible through the RPCs would be available. Except for some
specific scenarios in which the commands accessible through the RPCs are enough,
it is always convenient to link the snapshot protocol in the build system. In
our example, this can be done by invoking::

  $ ./scripts/link_protocol.sh src/proto_012_*

Alternatively, you can snapshot Alpha and link it with one single script:
``snapshot_alpha_and_link.sh``. This replaces steps 1 and 2. This script effectively
runs ``snapshot_alpha.sh`` and ``link_protocol.sh`` for you. In particular
it means you do not have to find the short hash of the protocol yourself
to pass it to ``link_protocol.sh``.
To run it, pass the protocol version number and name as follows::

  $ ./scripts/snapshot_alpha_and_link.sh 012 d


3. Set User-Activated Upgrade
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The currently active protocol supports self-amending through the voting procedure
of Tezos. However, such procedure needs to go through several voting periods
that involve several quorums of bakers, and we would rather test our migration
in a less involved way. Besides the amendments driven by the protocol, Tezos
also supports *user-activated* upgrades, which are triggered by the shell. The
user-activated upgrades allow the user to specify the level at which the next
protocol will be adopted, which can be used to perform emergency bug fixes, but
which is also useful to test migrations.

Depending on whether we test the migration on the sandbox or on a realistic
context imported from Mainnet, we would like to set the user-activated upgrades
respectively at a small level (some blocks after the genesis block at level
``0``) or at a high level (some blocks after the status imported from Mainnet,
which contains several hundreds of thousands of blocks). By convention, when
setting a user-activated upgrade the scripts would consider that the migration
is on the sandbox if the level is less or equal than ``28082``, and on a real
context imported from Mainnet otherwise, and the scripts would behave
differently.

If we are testing the migration on the sandbox, the user-activated upgrade
allows us to activate the predecessor of the Alpha protocol by using an
activation command after the sandbox starts, and to automatically trigger the
activation of the Alpha protocol when the sandbox reaches a given level. Using
this mechanism, we can start the sandbox, activate the predecessor of the Alpha
protocol, populate the empty context at will by using the shell of the
predecessor protocol, and then have the migration triggered automatically at the
desired level. The script ``scripts/user_activated_upgrade.sh`` receives the
path of the protocol to which we would like to upgrade, and the desired level.

In our example above, where the Alpha protocol was snapshot into
``src/proto_012_<short_hash>``, we can set the user-activated upgrade such that
the migration is triggered at level three by invoking::

  $ ./scripts/user_activated_upgrade.sh src/proto_012_* 3

If we had opted for not snapshotting the Alpha protocol, we could pass the path
``src/proto_alpha`` as the parameter of the command above.

Now we consider the case when testing the migration on a context imported from
the snapshot file. In that case, we should recall the level at which the
snapshot file was taken from the beginning of Section `Prepare the
migration`_. In our example, this level is ``1617344``. The user-activated
upgrade allows us to start the node imported from Mainnet, which would have the
predecessor of the Alpha protocol already active if the snapshot is recent
enough, and then have the migration triggered automatically at the desired
level, which has to be strictly bigger than the level at which the snapshot file
was taken.

In our example, where we the Alpha protocol was snapshot into
``src/proto_012_<short_hash>``, we can set the user-activated upgrade such that
the migration is triggered three levels after the level ``1617344`` at which the
snapshot was taken by invoking::

  $ ./scripts/user_activated_upgrade.sh src/proto_012_* 1617347

As before, if we had opted for not snapshotting the Alpha protocol, we could pass
the path ``src/proto_alpha`` as the parameter of the command above.

If we are testing the migration on an empty context on the sandbox, then we
should proceed directly to Section `5. Compile the project`_. Otherwise, the next
two subsections detail how to produce credentials that will allow us to make the
chain that we imported from Mainnet progress.


4. Patch the Shell to Obtain a Yes-Node
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we would start a node imported from Mainnet, how could we bake new blocks and
make the chain progress? We do not know the private keys of existing bakers in
Mainnet!

In order to produce credentials to make the chain imported from Mainnet
progress, we modify the code to produce a yes-node that forges and verifies
fake signatures. This can be achieved with a small patch to
``src/lib_crypto/signature.ml`` that replaces each signature with a
concatenation of a public key and a message, such that this fake signature is
still unique for each key and message. This patch is encoded as the git diff
contained in the file ``scripts/yes-node.patch``. We can apply this patch by
invoking::

  $ ./scripts/patch-yes_node.sh

5. Compile the Project
~~~~~~~~~~~~~~~~~~~~~~

At this point we have to compile the Alpha protocol (or the snapshot Alpha
protocol, in case we opted for it) that we will activate when running the
migration, as well as the shell if we patched it. We can compile the whole
project under the ``src`` folder by invoking::

  $ make


6. Import a Context From Mainnet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we wish to test the migration in a realistic scenario, we need to import a
context from a Mainnet's snapshot file. As explained in the beginning of Section
`Prepare the migration`_, in our example we will use a snapshot file
``~/snapshot-mainnet.rolling``
which was taken at level ``1617344``.

We also need to generate a node identity, which we will keep in the folder that
contains the history of the node. Since importing a node from a snapshot file is
very time consuming, once the node is imported and the identity is generated we
will keep the original folder unchanged, and we will copy its contents to a
fresh test folder every time we want to perform the migration.

For instance, the following commands import a context from the snapshot file
``~/snapshot-mainnet.rolling``
into the folder ``/tmp/octez-node-mainnet``,
and generate an identity in the same folder::

  $ ./octez-node snapshot import ~/snapshot-mainnet.rolling --data-dir /tmp/octez-node-mainnet
  $ ./octez-node identity generate --data-dir /tmp/octez-node-mainnet

The ``./octez-node snapshot import`` command accepts an option
``--block <block_hash>`` that instructs the command to check that the hash of
the last block in the imported chain is ``<block_hash>``. This mechanism helps
the developer to check that the imported chain contains blocks that are part of
the current main chain of the Tezos network. The snapshots websites normally provide
the hash of the last block in a given snapshot file. Although we will not be
using the ``--block`` option in this tutorial, the developer is encouraged to
check that this prefix corresponds to the hash of a real block in Mainnet.

Importing the context from a snapshot file is optional and should be performed
only if we want to test the migration on a realistic context from
Mainnet. Otherwise the migration will run on the sandbox.

7. Create a Yes-Wallet
~~~~~~~~~~~~~~~~~~~~~~

We also need to create a yes-wallet, which is a special wallet where secret
keys actually encode the same bytes as their corresponding public keys. By
adding to the yes-wallet the existing accounts of Mainnet bakers, we would have
enough rights to bake blocks at will. We can do so by running::

  $ dune exec devtools/yes_wallet/yes_wallet.exe -- create from context /tmp/octez-node-mainnet in /tmp/yes-wallet --active-bakers-only

This command creates a yes-wallet and places its folder in the
system's temp directory (in our example, ``/tmp``) as given by the path argument
``/tmp/yes-wallet``. If no path argument was given, the command would create the
yes-wallet folder in the default path ``./yes-wallet``.

The command above will generate a wallet containing approximately 400
keys. If you wish to restrict to a given percentage of the attesting
power by retrieving the first bakers (with the biggest staking
power), you can also use the ``--staking-share`` option to provide a
limit. For instance, the first largest bakers with an accumulated
stake of at least 75 percent can be kept with::

  $ dune exec devtools/yes_wallet/yes_wallet.exe -- create from context /tmp/octez-node-mainnet in /tmp/yes-wallet --active-bakers-only --staking-share 75

.. note::
   Prior to switching to the Tenderbake consensus algorithm it was
   sufficient to create a minimal yes-wallet with 8 Foundation
   keys. Starting from Protocol I this is no longer the case, because
   a number of bakers holding at least 2/3rds of the total attesting
   power have to attest a block for it to be considered valid.

By restricting the accumulated stake to 75% as in the command above,
the wallet is both "lighter" (it may contain around 30-40 keys and
therefore some commands like ``octez-client bake for`` will execute
faster) and its keys will represent more than the 2/3rds of the
attesting power for any given level.

Batch Steps 1--7 Above
~~~~~~~~~~~~~~~~~~~~~~

The script ``scripts/prepare_migration_test.sh`` batches steps 1--7 above.

The first parameter is optional and contains a name in the format
``<tag_starting_with_version_letter>_<version_number>``. If some name is passed,
then the Alpha protocol is snapshot into
``src/proto_<version_number>_<short_hash>``. If the name is omitted, then the
Alpha protocol in ``src/proto_alpha`` will be used for the migration testing.

Now the script takes the level at which we want to set the user-activated
upgrade. The script distinguishes whether the migration is on the sandbox or on
an imported context based on this level. (Recall that a level less or equal than
``28082`` corresponds to the sandbox, and a level greater than ``28082``
corresponds to an imported context.)  In our example, if we want to test the
migration on the sandbox and want to trigger it at level three, we can use::

  $ ./scripts/prepare_migration_test.sh d_012 3

If on the contrary we have imported a realistic context from the snapshot file
``~/snapshot-mainnet.rolling``
taken at level ``1617344``, and we want
to trigger the migration three levels after the level at which the snapshot file
was taken, we can use::

  $ ./scripts/prepare_migration_test.sh d_012 1617347 ~/snapshot-mainnet.rolling

In the latter case both the context and the yes-wallet folder will be placed in
the system's temp directory. In our example the temp directory is ``/tmp``, and
the context and yes-wallet would be placed in paths
``/tmp/octez-node-mainnet`` and ``/tmp/yes-wallet``
respectively.

If the script detects that the yes-wallet folder already exists int ``/tmp``,
then it will clean it by removing spurious files ``/tmp/yes-wallet/blocks`` and
``/tmp/yes-wallet/wallet_locks``, and it will not create a new yes-wallet
folder. If the script detects that the folder
``/tmp/octez-node-mainnet`` already exists, or if the developer
passes the path of a folder instead of the path of a snapshot file, then the
script will use the corresponding folder as the original folder, and will not
import a new context.

In case we opted for not snapshotting the Alpha protocol, we could batch steps
1--7 by respectively using the commands above, but omitting the name parameter
``d_012``.

The script ``scripts/prepare_migration_test.sh`` receives an optional
``<block_hash>`` as the last argument which, if passed, will be used for the
option ``--block <block_hash>`` to the ``./octez-node snapshot import`` command
when importing the context form Mainnet.

After performing the steps 1--7, the migration will be ready to be tested. The
next two subsections respectively detail how to run the migration on the sandbox
and on a context imported from Mainnet.


Run the Migration on the Sandbox
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we run the migration on an empty context, then we would start a sandboxed
node as usual after having prepared the migration test (see previous section).
In our example we can run the following::

  $ ./src/bin_node/octez-sandboxed-node.sh 1 --connections 0 &

We can also start the client::

  $ eval $(./src/bin_client/octez-init-sandboxed-client.sh 1)

Instead of command ``octez-activate-alpha``, the sandboxed client script
``src/bin_client/octez-init-sandboxed-client.sh`` now accepts a command
``octez-activate-XXX-<short_hash>`` that activates the predecessor protocol with
version number ``XXX`` and short hash ``<short_hash>``. In our example, the
predecessor protocol is ``011`` with short hash ``PtHangz2``. (Check the folder
``src`` for the version number and short hash of the predecessor protocol for
migrations to versions different from ``012``.) We can activate this protocol by
invoking::

  $ octez-activate-011-PtHangz2

Activation of the predecessor protocol produces one block and increases the
level by one. This unavoidable increase of the level has to be taken into
account when setting the desired level for the user-activated upgrade.

Now we can use the client commands to bake blocks until we reach the level at
which migration will be triggered, which in our example is ``3``. Since
activating the predecessor protocol increases the level by one, we need to bake
two more blocks::

  $ octez-client bake for --minimal-timestamp
  $ octez-client bake for --minimal-timestamp

.. note::
   Prior to Tenderbake activation (i.e. to the Protocol I) the command above
   requires a specific account to bake for. Any of ``bootstrap[0-9]`` accounts
   can be used to do it:

   ``$ octez-client bake for bootstrap1 --minimal-timestamp``

At this moment migration will be triggered and the protocol
``proto_012_<short_hash>`` will become active, and we will see the log message
``STITCHING!``.

The migration can be tested again by restarting the sandboxed node and client,
by activating the predecessor of the Alpha protocol, and by baking two blocks.


Run the Migration on a Context Imported From Mainnet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If we run the migration on a context imported from Mainnet, then we would start
the node using the context imported from the snapshot file. Since importing a
snapshot file is very time consuming, we will leave the original folder
unchanged, and every time we want to run the test, we will copy its contents to
a fresh test folder. In our example, we can do this by taking advantage of an
environment variable ``test_directory`` and the tool ``mktemp`` as follows::

  $ test_directory=$(mktemp -d -t "octez-node-mainnet-XXXX") && cp -r "/tmp/octez-node-mainnet/." "$test_directory"

This command creates a fresh test folder in the system's temp directory (in our
example ``/tmp``) whose name is ``octez-node-mainnet-XXXX``,
where the ``XXXX`` are four random alphanumerical characters, and sets the
environment variable ``test_directory`` to the path of the test folder, such
that we can run the node in the test folder later. Then it copies the contents
of the original context folder into the test folder.

Now, we can run the ``octez-node`` command by specifying the test folder
``$test_directory`` as the data directory. We will also specify the RPC address
``localhost``, such that the RPCs will be available at the url
``localhost:8732``. In our example, by invoking the following::

  $ ./octez-node run --synchronisation-threshold 0 --connections 0 --data-dir "$test_directory" --rpc-addr localhost &

We will now trigger the migration by baking blocks until the level reaches the
one specified when setting the user-activated upgrades. The blocks can be baked
with the yes-wallet created in step 5 above, and with any of the accounts
``foundation1`` to ``foundation8``. In our example, we can bake one block by
running the following command::

  $ ./octez-client -d /tmp/yes-wallet bake for --minimal-timestamp

.. note::
   Prior to Tenderbake activation (i.e. to the Protocol I) this command requires
   a specific account to bake for. Any of ``foundation[1-8]`` accounts can be
   used to do it.

   ``$ octez-client bake for foundation1 --minimal-timestamp``

   If the chosen account ``foundation1`` ceases to have the priority to bake, we
   can switch to any of the remaining accounts ``foundation2`` to
   ``foundation8``. We will always be able to make the chain progress since it is
   virtually impossible that at some moment all the eight accounts cease to have
   the priority to bake.

After baking three blocks the migration will be triggered and the protocol
``proto_012_<short_hash>`` will become active.  We will see the log message
``STITCHING!``.

The migration can be tested again by removing the test folder and the spurious
files ``blocks`` and ``wallet_lock`` in the yes-wallet folder. In our example we
can do this with the following command::

  $ rm -rf "$test_directory" && rm -f /tmp/yes-wallet/{blocks,wallet_lock}

Then we repeat the commands above in order to create a fresh test folder, and to
copy the context of the original folder into the test folder. In our example::

  $ test_directory=$(mktemp -d -t "octez-node-mainnet-XXXX") && cp -r "/tmp/octez-node-mainnet/." "$test_directory"

Now we run the node in the test folder by invoking::

  $ ./octez-node run --synchronisation-threshold 0 --connections 0 --data-dir "$test_directory" --rpc-addr localhost &

And finally, we bake the numbers of blocks specified by the user-activated
upgrade, with the command::

  $ ./octez-client -d /tmp/yes-wallet bake for --minimal-timestamp


Wrap up the Migration Procedure
-------------------------------

For convenience, this section collects all the steps needed to test the
migration, both on the sandbox and on a context imported from Mainnet.

Migration on the Sandbox
~~~~~~~~~~~~~~~~~~~~~~~~

Check out latest code::

  $ git checkout master
  $ git pull

Tweak migration by checking that
``src/proto_alpha/lib_protocol/init_storage.ml`` includes the following lines::

  | Hangzhou_011 ->
      Logging.log_notice "\nSTITCHING!\n" ;

Commit the feature::

  $ git commit -am 'My awesome feature'

Prepare migration by snapshotting the Alpha protocol, linking it to the build
system, setting user-activate upgrades, and compiling the project::

  $ ./scripts/prepare_migration_test.sh d_012 3

(Alternatively, each of these steps could be performed individually by invoking
the following fur commands)::

  $ ./scripts/snapshot_alpha.sh d_012
  $ ./scripts/link_protocol.sh src/proto_012_*
  $ ./scripts/user_activated_upgrade.sh src/proto_012_* 3
  $ make

Run sandboxed node and client::

  $ ./src/bin_node/octez-sandboxed-node.sh 1 --connections 0 &
  $ eval $(./src/bin_client/octez-init-sandboxed-client.sh 1)

Activate predecessor of the Alpha protocol and move chain one level forward::

  $ octez-activate-011-PtHangz2

Bake two more blocks::

  $ octez-client bake for --minimal-timestamp
  $ octez-client bake for --minimal-timestamp

You should see the ``STITCHING!`` message!

To test again, restart the sandboxed node and client::

  $ fg
  ./src/bin_node/octez-sandboxed-node.sh 1 --connections 0
  ^C
  $ ./src/bin_node/octez-sandboxed-node.sh 1 --connections 0 &
  $ eval $(./src/bin_client/octez-init-sandboxed-client.sh 1)

Activate predecessor of the Alpha protocol::

  $ octez-activate-011-PtHangz2

Bake two blocks::

  $ octez-client bake for --minimal-timestamp
  $ octez-client bake for --minimal-timestamp

You should see the ``STITCHING!`` message again!


Migration on a Context Imported From Mainnet
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Check out latest code::

  $ git checkout master
  $ git pull

Tweak migration by checking that
``src/proto_alpha/lib_protocol/init_storage.ml`` includes the
following lines::

  | Hangzhou_011 ->
      Logging.log_notice "\nSTITCHING!\n" ;

Commit the feature::

  $ git commit -am 'My awesome feature'

Prepare migration by snapshotting the Alpha protocol, linking it to the build
system, patching the shell in order to obtain yes-node, creating a yes-wallet,
setting user-activated upgrades, importing a context from Mainnet into the
original context folder, generating an identity in the same folder, and
compiling the project::

  $ ./scripts/prepare_migration_test.sh d_012 1617344 ~/mainnet.rolling

(Alternatively, each of these steps could be performed individually by
invoking the following eight commands)::

  $ ./scripts/snapshot_alpha.sh d_012
  $ ./scripts/link_protocol.sh src/proto_012_*
  $ ./scripts/user_activated_upgrade.sh src/proto_012_* 1617344
  $ ./scripts/patch-yes_node.sh
  $ make
  $ ./octez-node snapshot import ~/mainnet.rolling --data-dir /tmp/octez-node-mainnet
  $ ./octez-node identity generate --data-dir /tmp/octez-node-mainnet
  $ dune exec devtools/yes_wallet/yes_wallet.exe -- create from context /tmp/octez-node-mainnet in /tmp/yes-wallet --active-bakers-only

Copy original folder into test folder::

  $ test_directory=$(mktemp -d -t "octez-node-mainnet-XXXX") && cp -r "/tmp/octez-node-mainnet/." "$test_directory"

Run the node`::

  $ ./octez-node run --synchronisation-threshold 0 --connections 0 --data-dir "$test_directory" --rpc-addr localhost &

Bake three blocks::

  $ ./octez-client -d /tmp/yes-wallet bake for --minimal-timestamp
  $ ./octez-client -d /tmp/yes-wallet bake for --minimal-timestamp
  $ ./octez-client -d /tmp/yes-wallet bake for --minimal-timestamp

.. note::
   Prior to Tenderbake activation (i.e. to the Protocol I) this command requires
   a specific account to bake for. Any of ``foundation[0-9]`` accounts can be
   used to do it.

You should see the ``STITCHING!`` message!

To test again, kill the node::

  $ fg
  ./octez-node run --synchronisation-threshold 0 --connections 0 --data-dir "$test_directory" --rpc-addr localhost
  ^C

Clean up by removing test folder and copying original folder into fresh
test folder, and by removing files ``/tmp/yes-wallet/wallet_lock`` and
``/tmp/yes-wallet/blocks``::

  $ rm -rf "$test_directory" && rm -f /tmp/yes-wallet/{blocks,wallet_lock};
  $ test_directory=$(mktemp -d -t "octez-node-mainnet-XXXX") && cp -r "/tmp/octez-node-mainnet/." "$test_directory"

Run the node::

  ./octez-node run --synchronisation-threshold 0 --connections 0 --data-dir "$test_directory" --rpc-addr localhost &

And bake three blocks::

  $ ./octez-client -d /tmp/yes-wallet bake for --minimal-timestamp
  $ ./octez-client -d /tmp/yes-wallet bake for --minimal-timestamp
  $ ./octez-client -d /tmp/yes-wallet bake for --minimal-timestamp

You should see the ``STITCHING!`` message again!

Tips and Tricks
---------------

Migrating a context mostly concerns editing existing data structures.  For this
reason it is important to inspect the resulting context with the RPCs
``context/raw/json`` and ``context/raw/bytes``. The former RPC displays the json
value relative to a key of the context, using its json format. This is possible
thanks to the storage functors of Tezos, which are used to register every piece
of storage in a node and are aware of the json structure of the data. The latter
RPC is more low level and simply returns the bytes corresponding to a key. Both
RPCs support the option ``depth`` to control how much of the subtree of the key
should be displayed.

For example, if we use ``context/raw/json`` to inspect the size of the current
listings, which informs of how many rolls are allowed to vote in the current
period, we get::

  $ curl -s localhost:8732/chains/main/blocks/head/context/raw/json/votes/listings_size
  56639

On the other hand, if instead we use ``context/raw/bytes`` to inspect the data
corresponding to the same key, we obtain a string of bytes in hexadecimal
format::

  $ curl -s localhost:8732/chains/main/blocks/head/context/raw/bytes/votes/listings_size
  "0000dd3f"

This string of bytes can be converted using the OCaml toplevel to obtain the
same value retrieved before::

  utop # let h = 0x0000dd3f ;;
  val h : int = 56639

In our migration example above, we can inspect the json output of a specific
contract::

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

The ``raw/json`` interface conveniently hides the disk representation of data
and keys. Notice how the hashes of public keys are not stored as is, but instead
they are encrypted using the more efficient base58 format.

In this case, in order to inspect the low level representation in bytes, which
we would often need to, we have to convert hashes of public keys using ``tztop`` (utop adapted for protocol development)
and the functions ``of_b58check`` and ``to_b58check`` of module
``Contract_repr``::

  # let's borrow some code from the protocol tests
  $ dune exec -- tztop src/proto_alpha/lib_protocol/test/

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

On the other hand, we could have inspected the data corresponding to the same
key above with ``raw/bytes``, as we do below::

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

Observe that while the value in json format above shows a ``big_map`` field that
is empty (i.e. ``"big_map": [],``), the low-level representation of the same
value reveals that the field containing such an empty ``big_map`` is not stored
at all.


Anatomy of Migration Code
-------------------------

The migration code is triggered in ``init_storage.ml:prepare_first_block``, so
that function is the entry point to start reading it. Notice that constants are
migrated in ``raw_context.ml:prepare_first_block``, which takes a ``Context.t``
and returns a ``Raw_context.t`` containing the new constants. Migrating other
data can usually be done by manipulating the ``Raw_context.t``, and such code
should be placed in the match case ``Alpha_previous`` of
``init_storage.ml:prepare_first_block``.

Conversions of data structures from the previous protocol are typically found in
``storage.ml,i``, which may involve the functors in ``storage_functors.ml,i``.
Each migration is very custom, but there are two recurring schemas that emerged
over time.

For high-level changes, the interface offered by the ``storage_functors`` is
usually expressive enough. The migration would copy the code to read the data
structures in the previous version and simply rename it by adding a suffix with
the previous version number (in our example above where we are migrating to
version ``012``, the identifiers in the old code would be renamed by appending
the suffix ``_011`` to them). The values are then written using the code for the
data structures of the current protocol, thus performing the migration. The last
step in the migration would be to manually remove any remaining code with a
suffix corresponding to the previous version (``_011`` in our example).

Some migrations may require breaking the interface offered by the
``storage_functors``, and to modify the file ``raw_context.mli`` directly. In
this case we usually *copy* the data to a temporary path, perform the
conversion, and then *recursively remove* the temporary path.
