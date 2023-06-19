.. TODO tezos/tezos#2170: search shifted protocol name/number & adapt

.. _howtouse:

Getting started with Octez
==========================

This short tutorial illustrates the use of the various Octez binaries as well
as some concepts about the network.

.. _tezos_binaries:

The Binaries
------------

After a successful compilation, you should have the following binaries:

- ``octez-node``: the Octez daemon itself (see `Node`_);
- ``octez-client``: a command-line client and basic wallet (see `Client`_);
- ``octez-admin-client``: administration tool for the node (see :ref:`octez-admin-client`);
- ``octez-{baker,accuser}-*``: daemons to bake and accuse on the Tezos network (see :doc:`howtorun`);
- ``octez-signer``: a client to remotely sign operations or blocks
  (see :ref:`signer`);
- ``octez-smart-rollup-{client,node}-*``: executables for using and running a smart rollup as Layer 2 (see :doc:`../active/smart_rollups`)
- ``octez-smart-rollup-wasm-debugger``: debugger for smart rollup kernels (see :doc:`../active/smart_rollups`)
- ``octez-proxy-server``: a readonly frontend to ``octez-node`` designed to lower the load of full nodes (see :doc:`../user/proxy-server`)
- ``octez-codec``: a utility for documenting the data encodings and for performing data encoding/decoding (see `Codec`_)
- ``octez-protocol-compiler``: a domain-specific compiler for Tezos protocols (see `Protocol compiler`_)
- ``octez-snoop``: a tool for modeling the performance of any piece of OCaml code, based on benchmarking (see :doc:`../developer/snoop`)

The daemons other than the node are suffixed with the name of the protocol they are
bound to, and up to some version, also by its number.
For instance, ``octez-baker-PtNairob`` is the baker
for the Nairobi protocol, and ``octez-baker-alpha`` is the baker
of the development protocol.
The ``octez-node`` daemon is not suffixed by any protocol name, because it is independent of the economic protocol. See also the `Node's Protocol`_ section below.


Read The Manual
---------------

All the Octez binaries provide the ``--help`` option to display information about their usage, including the available options and the possible parameters.

Additionally, most of the above binaries (i.e., all but the node, the validator, and the compiler) provide a textual manual that can be obtained with the command ``man``,
whose verbosity can be increased with ``-v``, for example::

    octez-client man -v 3

It is also possible to get information on a specific command in the manual with ``man <command>``::

   octez-client man set

To see the usage of one specific command, you may also type the command without arguments, which display its possible completions and options::

   octez-client transfer

.. warning::

    Beware that the commands available on the client depend on the specific
    protocol run by the node. For instance, ``transfer`` is not available when
    the node runs the genesis protocol, which may happen for a few minutes when
    launching a node for the first time, **or when the client is not connected
    to a node**. In the last case, the above command generates a warning
    followed by an error::

        Warning:
          Failed to acquire the protocol version from the node
          [...]
        Error:
          Unrecognized command.
          Try using the man command to get more information.
        Usage:
          [...]

.. _octez_client_protocol:

To get the manual of a client command for a protocol other than that used by the node (or even when not connected to a node), use the option ``--protocol``, e.g.::

    octez-client --protocol ProtoALphaALph man transfer

Note that you can get the list of protocols known to the client with::

    octez-client list understood protocols

The full command line documentation of the Octez binaries supporting the ``man`` command is also available
online: :doc:`../shell/cli-commands`.

Node
----

The node is the main actor of the Tezos blockchain and it has two main
functions: running the gossip network and updating the context.
The gossip network is where all Tezos nodes exchange blocks and
operations with each other (see :ref:`octez-admin-client` to monitor
p2p connections).
Using this peer-to-peer network, an operation originated by a user can
hop several times through other nodes until it finds its way in a
block baked by a baker.
Using the blocks it receives on the gossip network the node also
keeps up to date the current *context*, that is the full state of
the blockchain shared by all peers.
Approximately every 30 seconds a new block is created and, when the node
receives it, it applies each operation in the block to its current
context and computes a new context.
The last block received on a chain is also called the *head* of that
chain.
Each new head is then advertised by the node to its peers,
disseminating this information to build a consensus across the
network.

Other than passively observing the network, your node can also inject
its own new operations when instructed by the ``octez-client`` and even
send new blocks when guided by the ``octez-baker-*``.
The node has also a view of the multiple chains that may exist
concurrently and selects the best one based on its fitness (see
:doc:`../active/consensus`).

.. note::

   The ``octez-node`` uses (unless the option ``--singleprocess`` is
   given) an auxiliary daemon in order to validate, apply and compute
   the resulting context of blocks, in parallel to its main
   process. Thus, an ``octez-validator`` process can appear while
   monitoring the active processes of the machine.

Node Identity
~~~~~~~~~~~~~

First, we need to generate a new identity for the node to
connect to the network::

    octez-node identity generate

.. note::

    If the node prompts you to install the Zcash parameter file, follow
    the :ref:`corresponding instructions <setup_zcash_params>`.

The identity comprises a pair of cryptographic
keys that nodes use to encrypt messages sent to each other, and an
antispam proof-of-work stamp proving that enough computing power has been
dedicated to creating this identity.
Note that this is merely a network identity and it is not related in
any way to a Tezos address on the blockchain.

If you wish to run your node on a test network, now is also a good time
to configure your node (see :ref:`builtin_networks`).

Node Synchronization
~~~~~~~~~~~~~~~~~~~~

Whenever a node starts, it tries to retrieve the most current head of the chain
from its peers. This can be a long process if there are many blocks to retrieve
(e.g. when a node is launched for the first time or has been out of sync for a
while), or on a slow network connection. The mechanism of :doc:`../user/snapshots` can
help in reducing the synchronization time.

Once the synchronization is complete, the node is said to be *bootstrapped*.
Some operations require the node to be bootstrapped.

.. _node-protocol:

Node's Protocol
~~~~~~~~~~~~~~~

A Tezos node can switch from one protocol to another during its
execution.  This typically happens during the synchronization phase
when a node launches for the first time. The node starts with the
genesis protocol and then goes through all previous protocols until it
finally switches to the current protocol.

Throughout the documentation, "Alpha" refers to the protocol in the
``src/proto_alpha`` directory of the ``master`` branch, that is, a protocol under development, which serves as a basis to propose replacements
for the currently active protocol. The Alpha protocol is used by
default in :doc:`sandbox mode <../user/sandbox>` and in the various test
suites.


Storage
~~~~~~~

All blockchain data is stored by the node under a data directory, which by default is ``$HOME/.tezos-node/``.

If for some reason your node is misbehaving or there has been an
upgrade of the network, it is safe to remove this directory, it just
means that your node will take some time to resync the chain.

If removing this directory, please note that if it took you a long time to
compute your node identity, keep the ``identity.json`` file and instead only
remove its child ``store``, ``context`` and ``protocol`` (if any) sub-directories.

If you are also running a baker, make sure that it is configured to access the
data directory of the node (see :ref:`how to run a baker <baker_run>`).


RPC Interface
~~~~~~~~~~~~~

The only programming interface to the node is through JSON RPC calls and it is disabled by
default.  More detailed documentation can be found in the :doc:`RPC index
<../active/rpc>`. The RPC interface must be enabled for the clients
to communicate with the node but it should not be publicly accessible on the
internet. With the following command, it is available uniquely on the
``localhost`` address of your machine, on the default port ``8732``.

::

   octez-node run --rpc-addr 127.0.0.1

Node configuration
~~~~~~~~~~~~~~~~~~

Many options of the node can be configured when running the node:

- RPC parameters (e.g. the port number for listening to RPC requests using option ``--rpc-addr``)
- The directory where the node stores local data (using option ``--data-dir``)
- Network parameters (e.g. the number of connections to peers, using option ``--connections``)
- Validator and mempool parameters
- :ref:`Logging options <configure_node_logging>`.

The list of configurable options can be obtained using the following command::

    octez-node run --help

You can read more about the :doc:`node configuration <../user/node-configuration>` and its :ref:`private mode <private-mode>`.

Besides listening from requests from the client,
the node listens to connections from peers, by default on port ``9732`` (this can be changed using option ``--net-addr``), so it's advisable to
open incoming connections to that port.

Summing up
~~~~~~~~~~

Putting together all the above instructions, you may want to run a node as follows:

.. code-block:: shell

    # Download a snapshot for your target network, e.g. <test-net>:
    wget <snapshot-url> -O <snapshot-file>
    # Configure the node for running on <test-net>:
    octez-node config init --data-dir ~/.tezos-node-<test-net> --network <test-net>
    # Import the snapshot into the node data directory:
    octez-node snapshot import --data-dir ~/.tezos-node-<test-net> --block <block-hash> <snapshot-file>
    # Run the node:
    octez-node run --data-dir ~/.tezos-node-<test-net> --rpc-addr 127.0.0.1

.. _howtouse_tezos_client:

Client
------

Octez client can be used to interact with the node, it can query its
status or ask the node to perform some actions.
For example, after starting your node you can check if it has finished
synchronizing (see :doc:`../shell/sync`) using::

   octez-client bootstrapped

This call will hang and return only when the node is synchronized
(recall that this is much faster when starting a node from a snapshot).
Once the above command returns,
we can check what is the current timestamp of the head of the
chain (time is in UTC so it may differ from your local time)::

   octez-client get timestamp

You can also use the above command before the node is bootstrapped, from another terminal.
However, recall that the commands available on the client depend on the specific
protocol run by the node. For instance, ``get timestamp`` isn't available when
the node runs the genesis protocol, which may happen for a few minutes when
launching a node for the first time.

A Simple Wallet
~~~~~~~~~~~~~~~

The client is also a basic wallet. We can, for example, generate a new pair of keys, which can be used locally
with the alias *alice*::

      $ octez-client gen keys alice

To check the account (also called a contract) for Alice has been created::

      $ octez-client list known contracts

You will notice that the client data directory (by default, ``~/.tezos-client``) has been populated with
3 files ``public_key_hashs``, ``public_keys`` and ``secret_keys``.
The content of each file is in JSON and keeps the mapping between
aliases (e.g., ``alice``) and the kind of keys indicated by the name
of each file.
Secret keys should be stored on disk encrypted with a password except when
using a hardware wallet (see :ref:`ledger`).
An additional file ``contracts`` contains the addresses of smart
contracts, which have the form *KT1…*.


Notice that by default, the keys were stored unencrypted, which is fine in our test example.
In more realistic scenarios, you should supply the option ``--encrypted`` when generating a new account::

      $ octez-client gen keys bob --encrypted

Tezos supports four different ECC (`Elliptic-Curve Cryptography <https://en.wikipedia.org/wiki/Elliptic-curve_cryptography>`_) schemes: *Ed25519*, *secp256k1* (the
one used in Bitcoin), *P-256* (also called *secp256r1*), and *BLS* (variant
*MinPk*, for aggregated signatures). The secp256k1 and P256
curves have been added for interoperability with Bitcoin and
Hardware Security Modules (*HSMs*) mostly. Unless your use case
requires those, you should probably use *Ed25519*. We use a verified
library for Ed25519, and it is generally recommended over other curves
by the crypto community, for performance and security reasons.

Make sure to make a back-up of the client data directory and that the password
protecting your secret keys is properly managed (if you stored them encrypted).

For more advanced key management we offer :ref:`ledger support
<ledger>` and a :ref:`remote signer<signer>`.

.. _using_faucet:

Get Free Test Tokens
~~~~~~~~~~~~~~~~~~~~

To test the networks and help users get familiar with the system, on
:doc:`test networks<test_networks>` you can obtain free tokens from
:ref:`a faucet <faucet>`. Transfer some to Alice's address.

Transfers and Receipts
~~~~~~~~~~~~~~~~~~~~~~

To fund our newly created account for Bob, we need to transfer some
tez using the *transfer* operation.
Every operation returns a *receipt* that recapitulates all the effects
of the operation on the blockchain.
A useful option for any operation is ``--dry-run``, which instructs
the client to simulate the operation without actually sending it to
the network, so that we can inspect its receipt.

Let's try::

  octez-client transfer 1 from alice to bob --dry-run

  Fatal error:
    The operation will burn ꜩ0.257 which is higher than the configured burn cap (ꜩ0).
     Use `--burn-cap 0.257` to emit this operation.

The client asks the node to validate the operation (without sending
it) and obtains an error.
The reason is that when we fund a new address we are also storing it
on the blockchain.
Any storage on chain has a cost associated to it which should be
accounted for either by paying a fee to a baker or by destroying
(``burning``) some tez.
This is particularly important to protect the system from spam.
Because storing an address requires burning ꜩ0.257 and the client has
a default of 0, we need to explicitly set a cap on the amount that we
allow to burn::

  octez-client transfer 1 from alice to bob --dry-run --burn-cap 0.257

This should do it and you should see a rather long receipt being
produced, here's an excerpt::

  ...
  Simulation result:
    Manager signed operations:
      From: tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w
      Fee to the baker: ꜩ0.001259
      ...
      Balance updates:
        tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w ............ -ꜩ0.001259
        fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,72) ... +ꜩ0.001259
      Revelation of manager public key:
        Contract: tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w
        Key: edpkuK4o4ZGyNHKrQqAox7hELeKEceg5isH18CCYUaQ3tF7xZ8HW3X
        ...
    Manager signed operations:
      From: tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w
      Fee to the baker: ꜩ0.001179
      ...
      Balance updates:
        tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w ............ -ꜩ0.001179
        fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,72) ... +ꜩ0.001179
      Transaction:
        Amount: ꜩ1
        From: tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w
        To: tz1Rk5HA9SANn3bjo4qMXTZettPjjKMG14Ph
        ...
        Balance updates:
          tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w ... -ꜩ1
          tz1Rk5HA9SANn3bjo4qMXTZettPjjKMG14Ph ... +ꜩ1
          tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w ... -ꜩ0.257

The client does a bit of magic to simplify our life and here we see
that many details were automatically set for us.
Surprisingly, our transfer operation resulted in **two** operations,
first a *revelation*, and then a transfer.
Alice's address, obtained from the faucet, is already present on the
blockchain, but only in the form of a *public key hash*
``tz1Rj...5w``.
To sign operations, Alice needs to first reveal the *public
key* ``edpkuk...3X`` behind the hash, so that other users can verify
her signatures.
The client is kind enough to prepend a reveal operation before the
first transfer of a new address, this has to be done only once, future
transfers will consist of a single operation as expected.

Another interesting thing we learn from the receipt is that there are
more costs being added on top of the transfer and the burn: *fees*.
To encourage a baker to include our operation, and in general
to pay for the cost of running the blockchain, each operation usually
includes a fee that goes to the baker.
Fees are variable over time and depend on many factors but the Octez
client selects a default for us.

The last important bit of our receipt is the balance updates that
resume which address is being debited or credited of a certain amount.
We see in this case that baker ``tz1Ke...yU`` is being credited one
fee for each operation, that Bob's address ``tz1Rk...Ph`` gets 1 tez
and that Alice pays the transfer, the burn, and the two fees.

Now that we have a clear picture of what we are going to pay we can
execute the transfer for real, without the ``dry-run`` option.
You will notice that the client hangs for a few seconds before
producing the receipt because after injecting the operation in your
local node it is waiting for it to be included by some baker on the
network.
Once it receives a block with the operation inside it will return the
receipt.

It is advisable to wait for several blocks to consider the transaction as
final.
Please refer to the :doc:`consensus algorithm documentation <../active/consensus>` and `analysis <https://research-development.nomadic-labs.com/faster-finality-with-emmy.html>`__ to better understand block finality in Tezos.
`This page <https://nomadic-labs.gitlab.io/emmyplus-experiments/>`__ provides concrete values for the number of blocks one should wait.

In the rare case when an operation is lost, how can we be sure that it
will not be included in any future block, and then we may re-emit it?
After 120 blocks a transaction is considered invalid and can't be
included anymore in a block.
Furthermore each operation has a counter that prevents replays so it is usually safe to re-emit an
operation that seems lost.


.. _originated-accounts:

Implicit Accounts and Smart Contracts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Tezos there are two kinds of accounts: *implicit accounts* and *smart contracts*.

- The implicit accounts are the addresses starting with *tz1*, *tz2*,
  and *tz3* we have used up to now. They are created with a transfer
  operation to the account public key hash.

- Smart contracts have addresses starting with *KT1* and are created
  with an origination operation. They don't have a corresponding
  secret key and they run Michelson code each time they receive a
  transaction.

Let's originate our first contract and call it *id*::

    octez-client originate contract id transferring 1 from alice \
                 running ./michelson_test_scripts/attic/id.tz \
                 --init '"hello"' --burn-cap 0.4

The initial balance is ꜩ1, generously provided by implicit account
*alice*. The contract stores a Michelson program ``id.tz``
(found in file :src:`michelson_test_scripts/attic/id.tz`), with
Michelson value ``"hello"`` as initial storage (the extra quotes are
needed to avoid shell expansion). The parameter ``--burn-cap``
specifies the maximal fee the user is willing to pay for this
operation, the actual fee being determined by the system.

A Michelson contract is expressed as a pure function, mapping a pair
``(parameter, storage)`` to a pair ``(list_of_operations, storage)``.
However, when this pure function is applied
to the blockchain state, it can
be seen as an object with a single method taking one parameter (``parameter``), and with a single attribute (``storage``).
The method updates the state (the storage), and submits operations as a side
effect.

For the sake of this example, here is the ``id.tz`` contract:

.. code-block:: michelson

    parameter string;
    storage string;
    code {CAR; NIL operation; PAIR};

It specifies the types for the parameter and storage, and implements a
function which updates the storage with the value passed as a parameter
and returns this new storage together with an empty list of
operations.


Gas and Storage Costs
~~~~~~~~~~~~~~~~~~~~~

A quick look at the balance updates on the receipt shows that on top of
funding the contract with ꜩ1, *alice* was also charged an extra cost
that is burnt.
This cost comes from the *storage* and is shown in the line
``Paid storage size diff: 46 bytes``, 41 for the contract and 5 for
the string ``"hello"``.
Given that a contract saves its data on the public blockchain that
every node stores, it is necessary to charge a fee per byte to avoid
abuse and encourage lean programs.

Let's see what calling a program with a new argument would look like
with the ``--dry-run`` option::

   octez-client transfer 0 from alice to id --arg '"world"' --dry-run

The transaction would successfully update the storage but this time it
wouldn't cost us anything more than the fee, the reason is that the
storage for ``"world"`` is the same as for ``"hello"``, which has
already been paid for.
To store more we'll need to pay more, you can try by passing a longer
string.

The other cost associated with running contracts is the *gas*, which
measures *how long* does a program take to compute.
Contrary to storage there is no cost per gas unit, a transfer can
require as much gas as it wants, however a baker that has to choose
among several transactions is much more likely to include a low gas
one because it's cheaper to run and validate.
At the same time, bakers also give priority to high fee transactions.
This means that there is an implicit cost for gas that is related to
the fee offered versus the gas and fees of other transactions.

If you are happy with the gas and storage of your transaction you can
run it for real, however it is always a good idea to set an explicit
limit for both. The transaction fails if any of the two limits are passed.
Note that the storage limit sets an upper bound to the storage size *difference*, so in our case, it may be 0 because our new value does not increase at all the storage size.

::

   octez-client transfer 0 from alice to id --arg '"world"' \
                                            --gas-limit 11375 \
                                            --storage-limit 0

A baker is more likely to include an operation with lower gas and
storage limits because it takes fewer resources to execute so it is in
the best interest of the user to pick limits that are as close as
possible to the actual use. In this case, you may have to specify some
fees (using option ``--fee``) as the baker is expecting some for the resource
usage. Otherwise, you can force a low fee operation using the
``--force-low-fee``, with the risk that no baker will include it.

More Michelson test scripts can be found in directory
:src:`michelson_test_scripts/`.
Advanced documentation of the smart contract language is available
:doc:`here<../active/michelson>`.


Validation
~~~~~~~~~~

The node allows validating an operation before submitting it to the
network by simply simulating the application of the operation to the
current context.
Without this mechanism, if you just send an invalid operation (e.g. sending more
tokens than you own), the node would broadcast it and when it is
included in a block you would have to pay the usual fee even if it won't
have an effect on the context.
To avoid this case the client first asks the node to validate the
transaction and only then sends it.

The same validation is used when you pass the option ``--dry-run``:
the receipt that you see is actually a simulated one.
The only difference is that, when this option is supplied, the transaction is not sent even if it proves to be valid.

Another important use of validation is to determine gas and storage
limits.
The node first simulates the execution of a Michelson program and
tracks the amount of gas and storage that has been consumed.
Then the client sends the transaction with the right limits for gas
and storage based on those indicated by the node.
This is why we were able to submit transactions without specifying
these limits: they were computed for us.

More information on validation can be found :doc:`here <../shell/validation>`.


It's RPCs all the Way Down
~~~~~~~~~~~~~~~~~~~~~~~~~~

The client communicates with the node uniquely through RPC calls so
make sure that the node is listening on the right ports and that the ports are
open.
For example the ``get timestamp`` command above is a shortcut for::

   octez-client rpc get /chains/main/blocks/head/header/shell

The client tries to simplify common tasks as much as possible, however
if you want to query the node for more specific information you'll
have to resort to RPCs.

.. _get_protocol_constants:

For example to check the value of important
:ref:`constants <protocol_constants>` in Tezos, which may differ between Mainnet and other
:ref:`test networks<test-networks>`, you can use::

   octez-client rpc get /chains/main/blocks/head/context/constants | jq
   {
     "proof_of_work_nonce_size": 8,
     "nonce_length": 32,
     ...
   }

Another interesting use of RPCs is to inspect the receipts of the
operations of a block::

  octez-client rpc get /chains/main/blocks/head/operations

It is also possible to review the receipt of the whole block::

  octez-client rpc get /chains/main/blocks/head/metadata

An interesting block receipt is the one produced at the end of a
cycle as many delegates receive back part of their unfrozen accounts.


You can find more info in the :doc:`RPCs' page <../active/rpc>`.

Environment variables for the client
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The behavior of the client can be configured using the following environment variables:

- ``TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER``: Setting this variable to "YES" (or: "yes", "Y", "y") disables the warning displayed by the client at startup when it is not launched on Mainnet.
- ``TEZOS_CLIENT_DIR``: This variable may be used to supply the client data directory (by default, ``~/.tezos-client``).
  Its value is overridden by option ``-d``.
- ``TEZOS_SIGNER_*``: These variables are used for connecting the client to a remote :ref:`signer <signer>` (see there for details).
- ``TEZOS_CLIENT_RPC_TIMEOUT_SECONDS``: This variable controls how long (in seconds, as an integer)
  the client will wait for a response from the node, for each of the two RPC calls made during startup.
  If this variable is not set, or otherwise cannot be parsed as a positive integer, a default value of ``10`` seconds is used for each call.
  The two RPC calls this variable affects are queries that the client makes to the node in order to determine:
  (1) the protocol version of the node it connects to, and (2) the commands supported in that version.
- ``TEZOS_CLIENT_REMOTE_OPERATIONS_POOL_HTTP_HEADERS``: This variable specifies
  custom HTTP headers to use with the ``--operations-pool`` option. Only the Host
  header is supported as of now (see description in `rfc2616, section 14.23
  <https://datatracker.ietf.org/doc/html/rfc2616#section-14.23>`_

Other binaries
--------------

In this short tutorial we will not use some other binaries, but let as briefly review their roles.

Codec
~~~~~

The Octez codec (``octez-codec``) is a utility that:

- provides documentation for all the encodings used in the ``octez-node`` (and other binaries), and
- allows to convert from JSON to binary and vice-versa for all these encodings.

It is meant to be used by developers for tests, for generating documentation when writing libraries that share data with the node, for light scripting, etc.
For more details on its usage, refer to its :ref:`online manual <codec_manual>` and to :doc:`../developer/encodings`.

Protocol compiler
~~~~~~~~~~~~~~~~~

The protocol compiler (``octez-protocol-compiler``) can compile protocols within the limited environment that the shell provides.
This environment is limited to a restricted set of libraries in order to constrain the possible behavior of the protocols.

It is meant to be used:

- by developers to compile the protocol under development,
- by the packaging process to compile protocols that are pre-linked in the binaries,
- by the Octez node when there is an on-chain update to a protocol that is not pre-linked with the binary.

Summary
-------

In this tutorial, you have learned:

- to start an Octez node and set up its basic configuration;
- to use the Octez client to create implicit accounts and do transfers between them;
- to deploy and interact with a simple predefined smart contract;
- to distinguish between the various costs associated to transactions such as burnt tez, fees, storage costs, and gas consumption;
- some further concepts such as transaction validation and the RPC interface;
- the role of other binaries, less frequently used than the client and the node.

You may now explore Tezos further, and enjoy using it!
