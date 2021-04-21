.. _howtouse:

How to use Tezos
================

This How To illustrates the use of the various Tezos binaries as well
as some concepts about the network.

.. _tezos_binaries:

The Binaries
------------

After a successful compilation, you should have the following binaries:

- ``tezos-node``: the tezos daemon itself;
- ``tezos-client``: a command-line client and basic wallet;
- ``tezos-admin-client``: administration tool for the node;
- ``tezos-{baker,endorser,accuser}-*``: daemons to bake, endorse and
  accuse on the Tezos network (see :ref:`howtorun`);
- ``tezos-signer``: a client to remotely sign operations or blocks
  (see :ref:`signer`);

The daemons are suffixed with the name of the protocol they are
bound to. For instance, ``tezos-baker-006-PsCARTHA`` is the baker
for the Carthage protocol, and ``tezos-baker-alpha`` is the baker
of the development protocol. See also the `Node Protocol`_ section below.


Read The Manual
---------------

The manual of each binary can be obtained with the command ``man`` and
the verbosity can be increased with ``-v``::

    tezos-client man -v 3

It is also possible to search a keyword in the manual with ``man <keyword>``::

   tezos-client man set

To use one specific command, type the command without arguments to see
possible completions and options::

   tezos-client transfer

However, beware that the commands available on the client depend on the specific
protocol run by the node. For instance, ``transfer`` is not available when
the node runs the genesis protocol, which may happen for a few minutes when
launching a node for the first time, or when the client is not connected to a
node. In the last case, the above command generates a warning::

    Warning:
      Failed to acquire the protocol version from the node

To get the manual of a command for a protocol other than that used by the node (or even when not connected to a node), use the option ``--protocol``, e.g.::

    tezos-client --protocol ProtoALphaALph man transfer

Note that you can get the list of protocols known to the client with::

    tezos-client list understood protocols

The full command line documentation of the Tezos client is also available
online: :ref:`client_manual`.

Node
----

The node is the main actor of the Tezos blockchain and it has two main
functions: running the gossip network and updating the context.
The gossip network is where all Tezos nodes exchange blocks and
operations with each other (see :ref:`tezos-admin-client` to monitor
p2p connections).
Using this peer-to-peer network, an operation originated by a user can
hop several times through other nodes until it finds its way in a
block baked by a baker.
Using the blocks it receives on the gossip network the shell also
keeps up to date the current `context`, that is the full state of
the blockchain shared by all peers.
Approximately every minute a new block is created and, when the shell
receives it, it applies each operation in the block to its current
context and computes a new context.
The last block received on a chain is also called the `head` of that
chain.
Each new head is then advertised by the node to its peers,
disseminating this information to build a consensus across the
network.

Other than passively observing the network, your node can also inject
its own new operations when instructed by the ``tezos-client`` and even
send new blocks when guided by the ``tezos-baker-*``.
The node has also a view of the multiple chains that may exist
concurrently and selects the best one based on its fitness (see
:ref:`proof-of-stake`).


Node Identity
~~~~~~~~~~~~~

First, we need to generate a new identity for the node to
connect to the network::

    tezos-node identity generate

The identity comprises a pair of cryptographic
keys that nodes use to encrypt messages sent to each other, and an
antispam-PoW stamp proving that enough computing power has been
dedicated to creating this identity.
Note that this is merely a network identity and it is not related in
any way to a Tezos address on the blockchain.

If you wish to run your node on a test network, now is also a good time
to configure your node (see :ref:`multinetwork`).

Node Synchronization
~~~~~~~~~~~~~~~~~~~~

Whenever a node starts, it tries to retrieve the most current head of the chain
from its peers. This can be a long process if there are many blocks to retrieve
(e.g. when a node is launched for the first time or has been out of sync for a
while), or on a slow network connection. The mechanism of :ref:`snapshots` can
help in reducing the synchronization time.

Once the synchronization is complete, the node is said to be *bootstrapped*.
Some operations require the node to be bootstrapped.

.. _node-protocol:

Node Protocol
~~~~~~~~~~~~~

A Tezos node can switch from one protocol to another during its
execution.  This typically happens during the synchronization phase
when a node launches for the first time. The node starts with the
genesis protocol and then goes through all previous protocols until it
finally switches to the current protocol.

Throughout the documentation, `Alpha` refers to the protocol in the
``src/proto_alpha`` directory of the ``master`` branch, that is, a protocol under development, which serves as a basis to propose replacements
for the currently active protocol. The Alpha protocol is used by
default in :ref:`sandbox mode<sandboxed-mode>` and in the various test
suites. Its git history is also more detailed.


Storage
~~~~~~~

All blockchain data is stored under ``$HOME/.tezos-node/``.

If for some reason your node is misbehaving or there has been an
upgrade of the network, it is safe to remove this directory, it just
means that your node will take some time to resync the chain.

If removing this directory, please note that if it took you a long time to
compute your node identity, keep the ``identity.json`` file and instead only
remove the child ``store`` and ``context`` directories.

If you are also running a baker, make sure that it has access to the
``.tezos-node`` directory of the node.


RPC Interface
~~~~~~~~~~~~~

The only interface to the node is through JSON RPC calls and it is disabled by
default.  More detailed documentation can be found in the :ref:`RPC index.
<rpc>` The RPC interface must be enabled for the clients
to communicate with the node but it should not be publicly accessible on the
internet. With the following command, it is available uniquely on the
`localhost` address of your machine, on the default port ``8732``.

::

   tezos-node run --rpc-addr 127.0.0.1

Node configuration
~~~~~~~~~~~~~~~~~~

Many options of the node can be configured when running the node:

- RPC parameters (e.g. the port number for listening to RPC requests using option ``--rpc-addr``)
- The directory where the node stores local data (using option ``--data-dir``)
- Network parameters (e.g. the number of connections to peers, using option ``--connections``)
- Validator and mempool parameters
- :ref:`Logging options <configure_logging>`.

The list of configurable options can be obtained using the following command::

    tezos-node run --help

You can read more about the :ref:`node configuration <node-conf>` and its :ref:`private mode <private-mode>`.

The node listens to connections from peers on port ``9732``, so it's advisable to
open incoming connections to that port.

Client
------

Tezos client can be used to interact with the node, it can query its
status or ask the node to perform some actions.
For example, after starting your node you can check if it has finished
synchronizing using::

   tezos-client bootstrapped

This call will hang and return only when the node is synchronized.
We can now check what is the current timestamp of the head of the
chain (time is in UTC so it may differ from your local time)::

   tezos-client get timestamp

However, recall that the commands available on the client depend on the specific
protocol run by the node. For instance, ``get timestamp`` isn't available when
the node runs the genesis protocol, which may happen for a few minutes when
launching a node for the first time.

A Simple Wallet
~~~~~~~~~~~~~~~

The client is also a basic wallet and after the activation above you
will notice that the directory ``.tezos-client`` has been populated with
3 files ``public_key_hashs``, ``public_keys`` and ``secret_keys``.
The content of each file is in JSON and keeps the mapping between
aliases (``alice`` in the subsequent commands) and what you would expect from the name
of the file.
Secret keys are stored on disk encrypted with a password except when
using a hardware wallet (see :ref:`ledger`).
An additional file ``contracts`` contains the addresses of smart
contracts, which have the form *KT1…*.

We can, for example, generate a new pair of keys, which can be used locally
with the alias *bob*::

      $ tezos-client gen keys bob

To check the contract has been created::

      $ tezos-client list known contracts

Tezos support three different ECC schemes: *Ed25519*, *secp256k1* (the
one used in Bitcoin), and *P-256* (also called *secp256r1*). The two
latter curves have been added for interoperability with Bitcoin and
Hardware Security Modules (*HSMs*) mostly. Unless your use case
requires those, you should probably use *Ed25519*. We use a verified
library for Ed25519, and it is generally recommended over other curves
by the crypto community, for performance and security reasons.

Make sure to make a back-up of this directory and that the password
protecting your secret keys is properly managed.

For more advanced key management we offer :ref:`ledger support
<ledger>` and a :ref:`remote signer<signer>`.


.. _faucet:

Get Free Tez
~~~~~~~~~~~~

To test the networks and help users get familiar with the system, on
:doc:`test networks<test_networks>` you can obtain free tez from a
`faucet <https://faucet.tzalpha.net>`__.

This will provide a wallet in the form of a JSON file
``tz1__xxxxxxxxx__.json``, that can be activated with the following
command::

    tezos-client activate account alice with "tz1__xxxxxxxxx__.json"

If you use the ``tezos-docker-manager.sh`` script (renamed as ``edo2net.sh``
to run the Edo2net test network for instance), you should prefix the file
with ``container:`` in order to copy it into the docker image:
``./edo2net.sh client activate account alice with "container:tz1__xxxxxxxxx__.json"``

Let's check the balance of the new account with::

    tezos-client get balance for alice

Please preserve the JSON file. It will be necessary in order to
reactivate the wallet when migrating between test networks, e.g., from
one protocol to the next, or in the event the test network is reset.

Please drink carefully and don't abuse the faucet: it only contains
30,000 wallets for a total amount of ꜩ760,000,000.


Transfers and Receipts
~~~~~~~~~~~~~~~~~~~~~~

To fund our newly created account, we need to transfer some
tez using the `transfer` operation.
Every operation returns a `receipt` that recapitulates all the effects
of the operation on the blockchain.
A useful option for any operation is ``--dry-run``, which instructs
the client to simulate the operation without actually sending it to
the network, so that we can inspect its receipt.

Let's try::

  tezos-client transfer 1 from alice to bob --dry-run

  Fatal error:
    The operation will burn ꜩ0.257 which is higher than the configured burn cap (ꜩ0).
     Use `--burn-cap 0.257` to emit this operation.

The client asks the node to validate the operation (without sending
it) and obtains an error.
The reason is that when we fund a new address we are also creating it
on the blockchain.
Any storage on chain has a cost associated to it which should be
accounted for either by paying a fee to a baker or by destroying
(`burning`) some tez.
This is particularly important to protect the system from spam.
Because creating an address requires burning ꜩ0.257 and the client has
a default of 0, we need to explicitly set a cap on the amount that we
allow to burn::

  tezos-client transfer 1 from alice to bob --dry-run --burn-cap 0.257

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
Surprisingly, our transfer operation resulted in `two` operations,
first a `revelation`, and then a transfer.
Alice's address, obtained from the faucet, is already present on the
blockchain, but only in the form of a `public key hash`
``tz1Rj...5w``.
To sign operations, Alice needs to first reveal the `public
key` ``edpkuk...3X`` behind the hash, so that other users can verify
her signatures.
The client is kind enough to prepend a reveal operation before the
first transfer of a new address, this has to be done only once, future
transfers will consist of a single operation as expected.

Another interesting thing we learn from the receipt is that there are
more costs being added on top of the transfer and the burn: `fees`.
To encourage a baker to include our operation, and in general
to pay for the cost of running the blockchain, each operation usually
includes a fee that goes to the baker.
Fees are variable over time and depend on many factors but the tezos
client selects a default for us.

The last important bit of our receipt is the balance updates that
resume which address is being debited or credited of a certain amount.
We see in this case that baker ``tz1Ke...yU`` is being credited one
fee for each operation, that Bob's address ``tz1Rk...Ph`` gets 1 tez
and that Alice pays the two fees, the transfer, and the burn.

Now that we have a clear picture of what we are going to pay we can
execute the transfer for real, without the dry-run option.
You will notice that the client hangs for a few seconds before
producing the receipt because after injecting the operation in your
local node it is waiting for it to be included by some baker on the
network.
Once it receives a block with the operation inside it will return the
receipt.

It is advisable to wait for several blocks to consider the transaction as
final, for an important operation we advise to wait for 60 blocks.

In the rare case when an operation is lost, how can we be sure that it
will not be included in any future block and re-emit it?
After 60 blocks a transaction is considered invalid and can't be
included anymore in a block.
Furthermore each operation has a counter (explained in more detail
later) that prevents replays so it is usually safe to re-emit an
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

    tezos-client originate contract id transferring 1 from alice \
                 running ./tests_python/contracts/attic/id.tz \
                 --init '"hello"' --burn-cap 0.4

The initial balance is ꜩ1, generously provided by implicit account
*alice*. The contract stores a Michelson program ``id.tz``, with
Michelson value ``"hello"`` as initial storage (the extra quotes are
needed to avoid shell expansion). The parameter ``--burn-cap``
specifies the maximal fee the user is willing to pay for this
operation, the actual fee being determined by the system.

A Michelson contract is semantically a pure function, mapping a pair
``(parameter, storage)`` to a pair ``(list_of_operations, storage)``. It can
be seen equivalently as an object with a single method, and a single attribute.
The method updates the state (the storage), and submits operations as a side
effect.

For the sake of this example, here is the `id.tz` contract:

.. code-block:: michelson

    parameter string;
    storage string;
    code {CAR; NIL operation; PAIR};

It specifies the types for the parameter and storage, and implements a
function which updates the storage with the value passed as a parameter
and returns the storage unchanged together with an empty list of
operations.


Gas and Storage Cost Model
~~~~~~~~~~~~~~~~~~~~~~~~~~

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

   tezos-client transfer 0 from alice to id --arg '"world"' --dry-run

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

::

   tezos-client transfer 0 from alice to id --arg '"world"' \
                                            --gas-limit 11375 \
                                            --storage-limit 46

A baker is more likely to include an operation with lower gas and
storage limits because it takes fewer resources to execute so it is in
the best interest of the user to pick limits that are as close as
possible to the actual use. In this case, you may have to specify some
fees as the baker is expecting some for the resource
usage. Otherwise, you can force a low fee operation using the
`--force-low-fee`, with the risk that no baker will include it.

More test contracts can be found in directory
:src:`tests_python/contracts_007/`.
Advanced documentation of the smart contract language is available
:ref:`here<michelson>`.


Validation
~~~~~~~~~~

The node allows validating an operation before submitting it to the
network by simply simulating the application of the operation to the
current context.
In general, if you just send an invalid operation e.g. sending more
tokens that what you own, the node will broadcast it and when it is
included in a block you'll have to pay the usual fee even if it won't
have an effect on the context.
To avoid this case the client first asks the node to validate the
transaction and then sends it.

The same validation is used when you pass the option ``--dry-run``,
the receipt that you see is actually a simulated one.

Another important use of validation is to determine gas and storage
limits.
The node first simulates the execution of a Michelson program and
tracks the amount of gas and storage that has been consumed.
Then the client sends the transaction with the right limits for gas
and storage based on those indicated by the node.
This is why we were able to submit transactions without specifying
these limits: they were computed for us.

More information on validation can be found :ref:`here. <validation>`


It's RPCs all the Way Down
~~~~~~~~~~~~~~~~~~~~~~~~~~

The client communicates with the node uniquely through RPC calls so
make sure that the node is listening and that the ports are
correct.
For example the ``get timestamp`` command above is a shortcut for::

   tezos-client rpc get /chains/main/blocks/head/header/shell

The client tries to simplify common tasks as much as possible, however
if you want to query the node for more specific information you'll
have to resort to RPCs. For example to check the value of important
constants in Tezos, which may differ between Mainnet and other
:ref:`test networks<test-networks>`, you can use::

   tezos-client rpc get /chains/main/blocks/head/context/constants | jq
   {
     "proof_of_work_nonce_size": 8,
     "nonce_length": 32,
     "max_anon_ops_per_block": 132,
     "max_operation_data_length": 32768,
     "preserved_cycles": 5,
     "blocks_per_cycle": 4096,
     "blocks_per_commitment": 32,
     "blocks_per_roll_snapshot": 256,
     "blocks_per_voting_period": 32768,
     "time_between_blocks": [
       "60",
       "75"
     ],
     "endorsers_per_block": 32,
     "hard_gas_limit_per_operation": "400000",
     "hard_gas_limit_per_block": "4000000",
     "proof_of_work_threshold": "70368744177663",
     "tokens_per_roll": "10000000000",
     "michelson_maximum_type_size": 1000,
     "seed_nonce_revelation_tip": "125000",
     "origination_burn": "257000",
     "block_security_deposit": "48000000",
     "endorsement_security_deposit": "6000000",
     "block_reward": "0",
     "endorsement_reward": "0",
     "cost_per_byte": "1000",
     "hard_storage_limit_per_operation": "60000"
   }

Another interesting use of RPCs is to inspect the receipts of the
operations of a block::

  tezos-client rpc get /chains/main/blocks/head/operations

It is also possible to review the receipt of the whole block::

  tezos-client rpc get /chains/main/blocks/head/metadata

An interesting block receipt is the one produced at the end of a
cycle as many delegates receive back part of their unfrozen accounts.


You can find more info in the :ref:`RPCs' page. <rpc>`
