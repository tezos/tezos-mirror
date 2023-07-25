=============
 Mockup mode
=============

By default the ``octez-client`` described in the
:doc:`sandboxed node <sandbox>` needs a node running.
This page describes the *mockup* mode, a mode that works without
connecting to a node. For the moment, its features are more
limited than the default mode (see the :doc:`proxy mode<proxy>`
for an intermediate mode between default and mockup).

Motivation
==========

The main motivation for implementing the mockup mode is to provide developers of Tezos smart contracts with an easy local environment, offering a fast development cycle, which uses only lightweight local files for keeping the state, and which does not require a running blockchain.

Overview
========

In mockup mode, the client uses some dummy values for the initial parameters that
are usually gathered from a node, such as the head of the chain or the network
identifier. Then the mockup client simulates activation from genesis and runs
local implementations of the RPCs itself.

Modes of operation
------------------

Mockup mode can run in three ways:

- Stateless mode.

  In this mode, octez-client operates on its inputs and returns a value. Nothing is written to disk, and no state is preserved between calls to the client. This is the default.
- Stateful mode.

  In this mode, octez-client creates or manipulates a state on disk. The switch for this is ``--base-dir <directory_name>``.
- Stateful asynchronous mode.

  This mode adds baking. The command-line switch for this is ``--base-dir <directory_name> --asynchronous``.

Supported features
------------------

Currently, depending on the way it is run, the mockup mode can:

* typecheck, serialize, sign and evaluate a contract -- without a node.

  These features do not require a persistent state.
* perform transactions, originations, contract calls in a purely local fashion;
  mimicking the sandboxed mode but without a node.

  These features require a persistent state.
* perform some RPCs locally via the command line.

  These features work in stateful asynchronous mode.

We recommend that beginners use the persistent state, as it represents a suitable tradeoff between simplicity and supported features.

Nevertheless, the modes of operation are shown in the order above.

Run a mockup client without persistent state
============================================

Without persistent state, the mockup mode is already able to
typecheck scripts. Let's typecheck for example the script :src:`michelson_test_scripts/mini_scenarios/hardlimit.tz`:

.. code-block:: shell-session

    $ octez-client --mode mockup typecheck script michelson_test_scripts/mini_scenarios/hardlimit.tz

The script can also be executed:

.. code-block:: shell-session

   $ octez-client --mode mockup run script <filename> on storage <storage> and input <input>

where ``<storage>`` and ``<input>`` are some :ref:`Michelson expression
<michelson_type_system>` describing the contract's storage and script input
respectively. A ``--trace-stack`` option can be added in the end to output the
state of the stack after each step of script's execution.

For example:

.. code-block:: shell-session

  $ octez-client --mode mockup run script michelson_test_scripts/attic/id.tz on storage '"hello"' and input '"world"'
  # Ignore warnings about the missing/wrong base directory, they do not apply to "run script"
  storage
    "world"
  [...]

Run a mockup client with persistent state
=========================================

Setting up a mockup state requires to choose a protocol.
To see the list of supported protocols in mockup mode, issue the
following command:

.. code-block:: shell-session

    $ octez-client list mockup protocols

At any given time, it should return ``Alpha`` and at least the two protocols before that.

To create the mockup client state, issue the following command:

.. code-block:: shell-session

    $ octez-client \
      --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
      --base-dir /tmp/mockup \
      --mode mockup \
      create mockup

Now that this command has been issued, the next calls below **all** use
``--mode mockup`` and ``--base-dir /tmp/mockup`` arguments. This is
akin to doing a mockup *session*. To avoid mistakes, we advise to
do the following in the local shell running the session:

.. code-block:: shell-session

    $ alias mockup-client='octez-client --mode mockup --base-dir /tmp/mockup'

You can now use standard commands, such as:

.. code-block:: shell-session

    $ mockup-client list known addresses
    bootstrap5: tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv (unencrypted sk known)
    bootstrap4: tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv (unencrypted sk known)
    bootstrap3: tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU (unencrypted sk known)
    bootstrap2: tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN (unencrypted sk known)
    bootstrap1: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx (unencrypted sk known)

.. code-block:: shell-session

    $ mockup-client transfer 100 from bootstrap1 to bootstrap2
    Node is bootstrapped, ready for injecting operations.
    Estimated gas: 10207 units (will add 100 for safety)
    Estimated storage: no bytes added
    Operation successfully injected in the node.
    Operation hash is 'ooMyN7FDmDGyNk8CLdSFwcdxcQea5KLXYqrgzu6CEYB7G2xYbth'
    NOT waiting for the operation to be included.
    Use command
      octez-client wait for ooMyN7FDmDGyNk8CLdSFwcdxcQea5KLXYqrgzu6CEYB7G2xYbth to be included --confirmations 30 --branch BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU
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

.. code-block:: shell-session

    $ mockup-client get balance for bootstrap1
    3999898.997437 ꜩ

One can also originate contracts:

.. code-block:: shell-session

    $ mockup-client originate contract foo transferring 100 from bootstrap1 running 'parameter unit; storage unit; code { CAR; NIL operation; PAIR}' --burn-cap 10
    [...]
    New contract KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat originated.

The client can be used to display the state of the contract, e.g. its storage:

.. code-block:: shell-session

    $ mockup-client get contract storage for foo
    Unit

The RPC mechanism can also be conveniently used to access the state of the contract in JSON format:

.. code-block:: shell-session

    $ mockup-client rpc get /chains/main/blocks/head/context/contracts/KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat/storage
    { "prim": "Unit" }

The stateful mockup mode stores state data in a single ``context.json`` file, located under the ``mockup`` subdirectory of the base directory. In our running example, its absolute file name is ``/tmp/mockup/mockup/context.json``.

Tune mockup parameters
======================

The examples so far have used mockup mode’s default settings. Some use cases need a custom setup, so mockup mode lets us configure some initial parameters.

For simplicity, the mockup mode - like the sandboxed mode - uses
default values for the :ref:`protocol constants <protocol_constants>`. Such values are visible as follows (we recall
that ``mockup-client`` is an alias for ``octez-client``, see previous
section):

.. code-block:: shell-session

    $ mockup-client config show
    Default value of --bootstrap-accounts:
    ...
    Default value of --protocol-constants:
    ...

To tune these values, we recommend to first generate the files
corresponding to the default values:

.. code-block:: shell-session


    $ mockup-client config init
    Written default --bootstrap-accounts file: /tmp/mockup/bootstrap-accounts.json
    Written default --protocol-constants file: /tmp/mockup/protocol-constants.json

You can now edit the files ``bootstrap-accounts.json`` and
``protocol-constants.json`` to your liking then create a tuned mockup state.

.. code-block:: shell-session

   $ mv /tmp/mockup/{bootstrap-accounts,protocol-constants}.json /tmp/.
   $ rm /tmp/mockup -Rf
   $ mockup-client --protocol ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK \
     create mockup \
     --protocol-constants /tmp/protocol-constants.json \
     --bootstrap-accounts /tmp/bootstrap-accounts.json

You can check your custom parameters were taken into account:

.. code-block:: shell-session

    $ mockup-client config show
    Default value of --bootstrap-accounts:
    ...
    Default value of --protocol-constants:
    ...

Setting protocol constants for the mockup mode
==============================================

Let's look at the contents of the ``protocol-constants.json`` file as produced
by the ``--mode mockup config init`` and ``--mode mockup config show``
commands. The following was generated:

.. code-block:: JSON

   {
        "preserved_cycles": 2,
        "blocks_per_cycle": 8,
        "blocks_per_commitment": 4,
        "nonce_revelation_threshold": 4,
        "blocks_per_stake_snapshot": 4,
        "cycles_per_voting_period": 8,
        "hard_gas_limit_per_operation": "1040000",
        "hard_gas_limit_per_block": "2600000",
        "proof_of_work_threshold": "4611686018427387903",
        "minimal_stake": "6000000000",
        "vdf_difficulty": "50000",
        "origination_size": 257,
        "reward_weights": {
            "base_total_rewards_per_minute": "85007812",
            "baking_reward_fixed_portion_weight": 5120,
            "baking_reward_bonus_weight": 5120,
            "attesting_reward_weight": 10240,
            "liquidity_baking_subsidy_weight": 1280,
            "seed_nonce_revelation_tip_weight": 1,
            "vdf_revelation_tip_weight": 1
        },
        "cost_per_byte": "250",
        "hard_storage_limit_per_operation": "60000",
        "quorum_min": 2000,
        "quorum_max": 7000,
        "min_proposal_quorum": 500,
        "liquidity_baking_toggle_ema_threshold": 1000000000,
        "max_operations_time_to_live": 240,
        "minimal_block_delay": "1",
        "delay_increment_per_round": "1",
        "consensus_committee_size": 256,
        "consensus_threshold": 0,
        "minimal_participation_ratio": {
            "numerator": 2,
            "denominator": 3
        },
        "max_slashing_period": 2,
        "limit_of_delegation_over_baking": 19,
        "percentage_of_frozen_deposits_slashed_per_double_baking": 10,
        "percentage_of_frozen_deposits_slashed_per_double_attestation": 50,
        "cache_script_size": 100000000,
        "cache_stake_distribution_cycles": 8,
        "cache_sampler_state_cycles": 8,
        "dal_parametric": {
            "feature_enable": false,
            "number_of_slots": 16,
            "attestation_lag": 4,
            "attestation_threshold": 50,
            "blocks_per_epoch": 2,
            "redundancy_factor": 8,
            "page_size": 128,
            "slot_size": 32768,
            "number_of_shards": 64
        },
        "smart_rollup_enable": true,
        "smart_rollup_arith_pvm_enable": false,
        "smart_rollup_origination_size": 6314,
        "smart_rollup_challenge_window_in_blocks": 80640,
        "smart_rollup_stake_amount": "10000000000",
        "smart_rollup_commitment_period_in_blocks": 60,
        "smart_rollup_max_lookahead_in_blocks": 172800,
        "smart_rollup_max_active_outbox_levels": 80640,
        "smart_rollup_max_outbox_messages_per_level": 100,
        "smart_rollup_number_of_sections_in_dissection": 32,
        "smart_rollup_timeout_period_in_blocks": 40320,
        "smart_rollup_max_number_of_cemented_commitments": 5,
        "smart_rollup_max_number_of_parallel_games": 32,
        "smart_rollup_reveal_activation_level": {
            "raw_data": { "Blake2B": 0 },
            "metadata": 0,
            "dal_page": 0
        },
        "zk_rollup_enable": false,
        "zk_rollup_origination_size": 4000,
        "zk_rollup_min_pending_to_process": 10,
        "zk_rollup_max_ticket_payload_size": 2048,
        "global_limit_of_staking_over_baking": 5,
        "edge_of_staking_over_delegation": 2,
        "adaptive_issuance_launch_ema_threshold": 1600000000,
        "adaptive_rewards_params": {
            "reward_ratio_min": { "numerator": "1", "denominator": "200" },
            "reward_ratio_max": { "numerator": "1", "denominator": "10" },
            "max_bonus": "50000000000000",
            "growth_rate": "115740740",
            "center_dz": { "numerator": "1", "denominator": "2" },
            "radius_dz": { "numerator": "1", "denominator": "50" }
        },
        "chain_id": "NetXynUjJNZm7wi",
        "initial_timestamp": "1970-01-01T00:00:00Z"
  }

Besides usual protocol constants, there are 2 additional fields supported in Mockup mode:

* ``chain_id``: Used to prevent replay of operations between chains. You can pick a chain id for your mockup environment using the following command:

.. code-block:: shell-session

   $ octez-client compute chain id from seed <string>

For instance, the following command:

.. code-block:: shell-session

   $ octez-client compute chain id from seed strudel

yields the chain id ``NetXwWbjfCqBTLV``.


* ``initial_timestamp``: The creation time of the first block
  of the chain. This date string follows the ISO-8601 standard format, which can be
  generated by ``date --iso-8601=seconds``.


Baking
======

Baking in mockup mode is more aptly named *fake baking*. Indeed, it behaves
somewhat differently than baking in the sandbox.

With fake baking, everything happens locally, keeping track on disk of the
context and the mempool. In addition, the mockup chain only ever has *one* live
block, its head, so that you cannot have competing chains. In effect, it behaves
as if the time-to-live of transactions was 0.

As a result of only having one block, only transactions done on the head can be
baked in. Consequently, transactions refused during successful baking will not
be in position to be added at any point down the road. Thus, after each
successful baking, the mempool is emptied from any outstanding operations, which
are appended to a so-called *trashpool* containing the list of all refused
transactions at any point.

Let us make that clearer with an example.

Run a mockup client with asynchronous state
===========================================

We will start by creating a mockup
directory supporting *asynchronous* transfers, i.e., where transfers do not
immediately bake the block.

.. code-block:: shell-session

   $ rm /tmp/mockup -Rf # Was created by commands above
   $ mockup-client create mockup --asynchronous

This will create a fresh mockup directory.  Notice that, in addition to the
``mockup/context.json`` file, you now also have a ``mockup/mempool.json``, which
is initially empty.

Now, let us add 2 transactions, that we will label respectively ``t1`` and
``t2``, to the mempool.

.. code-block:: shell-session

   $ mockup-client transfer 1 from bootstrap1 to bootstrap2 --fee 1
   $ mockup-client transfer 2 from bootstrap2 to bootstrap3 --fee 0.5

You can check that it is indeed the
case by visiting ``mockup/mempool.json``. This should look like this

.. code-block:: JSON

   [ { "shell_header":
         { "branch": "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU" },
       "protocol_data":
         { "contents":
             [ { "kind": "transaction",
                 "source": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
                 "fee": "1000000", "counter": "1", "gas_limit": "10307",
                 "storage_limit": "0", "amount": "1000000",
                 "destination": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN" } ],
           "signature":
             "siggZXnjqYnFMjMxfE1avK2PZdRmRekp5fr56F5uJcuQkfHPL23HNDdtz2iG1QeYtU8DGEniWXjqDh1RxGx6scVgMaK74CrF" } },
     { "shell_header":
         { "branch": "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU" },
       "protocol_data":
         { "contents":
             [ { "kind": "transaction",
                 "source": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
                 "fee": "500000", "counter": "1", "gas_limit": "10307",
                 "storage_limit": "0", "amount": "2000000",
                 "destination": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" } ],
           "signature":
             "sigTBpkXw6tC72L2nJ2r2Jm5iB6uidTWqoMNd4oEawUbGBf5mHVfKawFYL8X8MJECpL73oBnfujyUZNLK2LQWD1FaCkYMP4j" } } ]

Now let's simulate a selective baker, like so

.. code-block:: shell-session

   $ mockup-client bake for bootstrap1 --minimal-fees 0.6

The effect of successfully baking the new head will be to include ``t1`` but
discard ``t2``. You can check that ``t2`` has been added to the file
``mockup/trashpool.json``, since we know it cannot be added to further
blocks of the mockup chain.

.. code-block:: JSON

   [ { "shell_header":
      { "branch": "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU" },
    "protocol_data":
      { "contents":
          [ { "kind": "transaction",
              "source": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
              "fee": "500000", "counter": "1", "gas_limit": "10307",
              "storage_limit": "0", "amount": "2000000",
              "destination": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" } ],
        "signature":
          "sigTBpkXw6tC72L2nJ2r2Jm5iB6uidTWqoMNd4oEawUbGBf5mHVfKawFYL8X8MJECpL73oBnfujyUZNLK2LQWD1FaCkYMP4j" } } ]

If we repeat somewhat similar steps

.. code-block:: shell-session

   $ mockup-client transfer 1 from bootstrap4 to bootstrap5 --fee 1
   $ mockup-client transfer 2 from bootstrap2 to bootstrap3 --fee 0.5

And bake once more selectively

.. code-block:: shell-session

   $ mockup-client bake for bootstrap3 --minimal-fees 0.6

Then, once again, the first transaction, with a fee of 1, will make it as part
of the new head whereas the second will be appended to the trashpool, which now
looks like this

.. code-block:: JSON

   [ { "shell_header":
      { "branch": "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU" },
    "protocol_data":
      { "contents":
          [ { "kind": "transaction",
              "source": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
              "fee": "500000", "counter": "1", "gas_limit": "10307",
              "storage_limit": "0", "amount": "2000000",
              "destination": "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" } ],
        "signature":
          "sigTBpkXw6tC72L2nJ2r2Jm5iB6uidTWqoMNd4oEawUbGBf5mHVfKawFYL8X8MJECpL73oBnfujyUZNLK2LQWD1FaCkYMP4j" } },
  { "shell_header":
      { "branch": "BKmdPRhxVBU4RCpHsLtU2FHNXRPCbcquMTpzK5QWvHG9C4TwMCj" },
    "protocol_data":
      { "contents":
          [ { "kind": "transaction",
              "source": "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
              "fee": "500000", "counter": "1", "gas_limit": "10307",
              "storage_limit": "0", "amount": "2000000",
              "destination": "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU" } ],
        "signature":
          "sigeFcabZTE8Y2LXv19Fe7TbRtkjzVpBy2qhABp263Xnj8TJtA6XpRRMfGeD5YxwCJiTr9r6ZFGBdLnpxL9Y9CG3bpbXmu7E" } } ]

Performing protocol migrations of persistent mockup states
==========================================================

The persistent state of the mockup mode is highly protocol-dependent.
But Tezos is self-amending: protocols regularly evolve from one to the next.
When a protocol switch happens on-chain, the protocol state is automatically
migrated to the format used by the new protocol.

A command is provided to do the same on the persistent mockup state:

::

   $ mockup-client migrate mockup to <protocol hash>

The protocol corresponding to the hash must know how to migrate from the current protocol.

This is mostly useful for protocol developers, but also for other developers, e.g., those wanting to check the robustness of their application with respect to protocol changes, including new features or breaking changes.

See also
========

This tutorial has also served as a base for `a nice blog post <https://research-development.nomadic-labs.com/introducing-mockup-mode-for-tezos-client.html>`__, written in a more casual way and intended for a larger audience of application developers.
Of course, some aspects may gradually become outdated in the blog version.
