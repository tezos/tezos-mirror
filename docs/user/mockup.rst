.. _mockup-mode:

Mockup mode
--------------

By default the ``tezos-client`` described in the
:ref:`sandboxed node<sandboxed-mode>` needs a node running.
This page describes the *mockup* mode, a mode that works without
connecting to a node. For the moment, its features are more
limited than the default mode.

In mockup mode, the client uses some dummy values for initial parameters that
are usually gathered from a node, such as the head of the chain or the network
identifier. Then the mockup client simulates activation from genesis and runs
local implementations of the RPCs itself.

The mockup mode can either use a volatile, in-memory environment or work
on a persistent state when ``--base-dir`` is specified at creation.

In the current state the mockup mode can:

* typecheck, serialize, sign and evaluate a contract -- without a node.
  These features do not require a persistent state.
* perform transactions, originations, contract calls in a purely local fashion;
  mimicking the sandboxed mode but without a node. These features
  require a persistent state.
* Perform some RPCs locally via `tezos-client rpc {get,post}`.

We recommend that beginners *always* use the persistent state, for simplicity.

Run a mockup client with persistent state
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To see the list of supported protocols in mockup mode, issue the
following command:

::

    $ tezos-client list mockup protocols

At the time of writing, two protocols are supported hence the output should
be the following (ignore the Warning if there is any):

::

    ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK
    PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb

To create the mockup client state, issue the following command:

::

    tezos-client --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
      --base-dir /tmp/mockup create mockup

Now that this command has been issued, the next calls below **all** use
``--mode mockup`` and ``--base-dir /tmp/mockup`` arguments. This is
akin to doing a mockup *session*. To avoid mistakes, we advise to
do the following in the local shell running the session:

::

    $ alias mockup-client='tezos-client --mode mockup --base-dir /tmp/mockup'

You can now use standard commands, such as:

::

    $ mockup-client list known addresses
    bootstrap5: tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv (unencrypted sk known)
    bootstrap4: tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv (unencrypted sk known)
    bootstrap3: tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU (unencrypted sk known)
    bootstrap2: tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN (unencrypted sk known)
    bootstrap1: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx (unencrypted sk known)

::

    $ mockup-client transfer 100 from bootstrap1 to bootstrap2
    Node is bootstrapped, ready for injecting operations.
    Estimated gas: 10207 units (will add 100 for safety)
    Estimated storage: no bytes added
    Operation successfully injected in the node.
    Operation hash is 'ooMyN7FDmDGyNk8CLdSFwcdxcQea5KLXYqrgzu6CEYB7G2xYbth'
    NOT waiting for the operation to be included.
    Use command
      tezos-client wait for ooMyN7FDmDGyNk8CLdSFwcdxcQea5KLXYqrgzu6CEYB7G2xYbth to be included --confirmations 30 --branch BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU
    and/or an external block explorer to make sure that it has been included.
    This sequence of operations was run:
      Manager signed operations:
        From: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
        Fee to the baker: ꜩ0.001282
        Expected counter: 2
        Gas limit: 10307
        Storage limit: 0 bytes
        Balance updates:
          tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ........... -ꜩ0.001282
          fees(tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU,0) ... +ꜩ0.001282
        Transaction:
          Amount: ꜩ100
          From: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
          To: tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN
          This transaction was successfully applied
          Consumed gas: 10207
          Balance updates:
            tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx ... -ꜩ100
            tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN ... +ꜩ100

::

    $ mockup-client get balance for bootstrap1
    3999898.997437 ꜩ

One can also originate contracts:

::

    $ mockup-client originate contract foo transferring 100 from bootstrap1 running 'parameter unit; storage unit; code { CAR; NIL operation; PAIR}' --burn-cap 10
    [...]
    New contract KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat originated.

The client can be used to display the state of the contract, eg its storage:

::

    $ mockup-client get contract storage for foo
    Unit

The RPC mechanism can also be conveniently used to access the state of the contract in JSON format:

::

    $ mockup-client rpc get /chains/main/blocks/head/context/contracts/KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat/storage
    { "prim": "Unit" }

Run a mockup client without persistent state
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Without persistent state, the mockup mode is still able to
typecheck scripts:::

    ./tezos-client --mode mockup typecheck script ./tests_python/contracts/mini_scenarios/hardlimit.tz

Tune mockup parameters
~~~~~~~~~~~~~~~~~~~~~~

To keep it simple, the mockup mode - like the sandboxed mode - uses
default values. Such values are visible as follows (we recall
that ``mockup-client`` is an alias for ``tezos-client``, see previous
section):

::

    $ mockup-client config show mockup
    Default value of --bootstrap-accounts:
    ...
    Default value of --protocol-constants:
    ...

To tune these values, we recommend to first generate the files
corresponding to the default values:

::

    $ mockup-client config init mockup
    Written default --bootstrap-accounts file: /tmp/mockup/bootstrap-accounts.json
    Written default --protocol-constants file: /tmp/mockup/protocol-constants.json

You can now edit the files ``bootstrap-accounts.json`` and
``protocol-constants.json`` to your liking then create a tuned mockup state.

::

   $ mv /tmp/mockup /tmp/mockup.old && \
     mockup-client --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
     create mockup for protocol \
     --protocol-constants protocol-constants.json \
     --bootstrap-accounts bootstrap-accounts.json

Setting protocol constants for the mockup mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's look at the contents of the ``protocol-constants.json`` file as produced
by the ``config init mockup`` of ``config show mockup`` commands. The following
was generated using the Carthage protocol:

.. code-block:: JSON

   { "hard_gas_limit_per_operation": "1040000",
     "hard_gas_limit_per_block": "10400000",
     "hard_storage_limit_per_operation": "60000",
     "cost_per_byte": "1000",
     "chain_id": "NetXynUjJNZm7wi",
     "initial_timestamp": "1970-01-01T00:00:00Z" }

By modifying the two first fields, a user can easily create a mockup environment
with bumped up (or down) gas limits and storage limit. A invariant should be
that the gas limit per block should be greater or equal to the gas limit per
operation.  The ``cost_per_byte`` is used to compute the amount of tokens to be
burnt proportionally to the fresh storage consumed by the execution of an
operation.  The ``chain_id`` is used to prevent replay of operations between
chains.  You can pick a chain id for your mockup environment using the following
command: 

::

   $ tezos-client compute chain id from seed <string>

For instance, the following command:

::

   $ tezos-client compute chain id from seed strudel

yields the chain id ``NetXwWbjfCqBTLV``.


The last field,  ``initial_timestamp``, is the creation time of the first block
of the chain. This date string follows the ISO-8601 standard format, which be
generated by `date --iso-8601=seconds`.
