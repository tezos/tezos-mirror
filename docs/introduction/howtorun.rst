Running Octez
=============

In this section, we discuss how to take part in the protocol that runs
the network.
There are three main ways to participate: delegating
your coins, staking, and running a delegate.
The main advantage of delegating your coins is simplicity.
Staking your coins is also as simple, but significantly increases the potential rewards modulo some slightly increased risks.
The third way allows to participate more actively in the protocol, by baking blocks and voting, but is more demanding; however, the extra effort is compensated by more rewards in tez.

To learn more about the protocol refer to :doc:`this page <../active/protocol_overview>`.

No matter how you decide to run Octez, your node must have an accurate time source and be properly synchronized to it, e.g. by configuring an NTP daemon.
This is especially important for bakers, as baking nodes desynchronized from the correct time of day have caused operational problems in the past by "baking in the future".

.. _delegating_coins:

Delegating your coins
---------------------

If you don't want to deal with the complexity of running your own
delegate, you can always take part in the protocol by delegating your
coins to one.

Both user accounts and smart contracts can have a
delegate. Setting or resetting the delegate of a user account is
achieved by the following command:

::

   octez-client set delegate for <user_account> to <delegate>

where ``<user_account>`` is the address or alias of the user
account to delegate and ``<delegate>`` is the address or alias of the
delegate (which has to be :ref:`registered<DelegateRegistration>`).

To stop a delegation, the following command can be used:

::

   octez-client withdraw delegate from <user_account>

Smart contract can also delegate the tokens they hold to registered
delegates. The initial delegate of a smart contract can be set at
origination using the ``--delegate`` option:

::

    octez-client originate contract <contract_alias> transferring <initial_balance> from <originator> running <script> --delegate <delegate> --burn-cap <cap>


Once the contract is originated, the only way to stop or modify its
delegation is by using the ``SET_DELEGATE`` Michelson instruction (see
`the Michelson documentation <https://tezos.gitlab.io/michelson-reference/#instr-SET_DELEGATE>`__ for more
details).


Notice that only user accounts can be delegates, so your delegate
must be a *tz* address.

Funds in user accounts which are not registered as delegates
do not participate in baking.

Note that delegating coins doesn't mean that a delegate can spend
them, they only add to its delegated balance.
In turn, delegators can freely spend their own funds in spite of the active delegation (they are not locked, like in other PoS algorithms).
Technically, delegation is a link between a delegator account and a delegate account, meaning that *all* the funds of the former are delegated to the latter, until the delegation is withdrawn.
When a delegator spends their tokens, the delegated balance of their delegate decreases; conversely, when they receive tokens the delegated balance of their delegate increases.

Since the activation of the new :doc:`staking mechanism <../active/staking>`,
33% of your total funds count towards your delegator's baking power (before the staking mechanism, 100% of your total funds counted towards your delegator baking power). The delegated funds still count for 100% for the voting power, just like before the activation of the new staking mechanism.

.. _staking_coins:

Staking your coins
------------------

Since the activation of the new :doc:`staking mechanism <../active/staking>`,
if you don't want to deal with the complexity of running your own
delegate, you can also take part in the protocol by staking part of your
coins to accrue to your delegate’s staking power.

Only user accounts (not smart contracts) can stake funds.

Funds can only be staked to your current delegate. Therefore, staking requires previously declaring a delegate.
Setting or resetting the delegate of a user account is done as for delegating, see above.

The delegate has no control over your stake: cannot spend it nor unstake it, so your deposit remains yours and under your control.
Staked coins cannot be spent by yourself, either; but you can unstake them later.
Your staked funds add to your delegate's staking balance (so staked funds count twice as much as delegated funds).
In turn, your staked funds are frozen and subject to slashing in case of misbehavior of your delegate such as double signing, similarly to the delegate's own staked funds.
In compensation of this risk assumed by stakers, they may receive some part of the delegate's rewards, the exact proportion being settable by the delegate.

As opposed to delegation (where you always delegate all your funds), as a staker you can choose a specific amount from your balance to stake, as follows::

  octez-client stake <amount> for <staker>

Later on, you can unstake part or all of your staked funds::

  octez-client unstake <amount|"everything"> for <staker>

The requested amount will immediately be unstaked, however it will remain frozen for some period (4 cycles), and only after that the funds are said to be both unstaked and finalizable.
At that point, the staker can make them spendable again as follows::

  octez-client finalize unstake for <staker>

For more details on the staking interface, see :ref:`staked_funds_management`.


Running a delegate
------------------

A delegate is responsible for baking blocks, attesting blocks and
accusing other delegates in case they try to double bake or double
attest. A delegate is also responsible for taking part in the
:doc:`governance process<../active/voting>`.

Rights for baking and attesting are randomly assigned
to delegates proportionally to their :doc:`baking power <../alpha/baking_power>`,
which usually is their own staked funds plus the funds staked by external stakers, plus the third of their total delegation.

Delegates are required to freeze some of their funds into a :ref:`security deposit <security_deposit>`.
This can be done via the same commands used by external stakers in the previous section.

.. _DelegateRegistration:

Register and check your rights
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To run a delegate, you first need to register as one using
your user account::

   octez-client register key mybaker as delegate

.. note::

   It is possible to also register a :ref:`consensus key <consensus_key_details>` and/or a :ref:`companion key <companion_key>` at this step.

You also need to stake some tez, at least ``MINIMAL_FROZEN_STAKE`` (see :ref:`ps_constants`), and to have at least ``MINIMAL_STAKE = 6000`` :ref:`baking power <minimal_baking_power>`, taking into account your own and all your delegators' staked balances, as well as their delegated balances with a lesser weight.

On testnets, when you obtain coins from :ref:`a faucet<faucet>`, if
you are lucky to obtain more than the minimum required to be a
delegate, you can stake the minimum amount by yourself and keep the rest for fees.
Otherwise, you need to ask the faucet for more accounts and delegate
them to the first.

Most commonly, you would stake (at least) the needed amount from your own tez, without waiting for delegators::

   octez-client stake 6000 for mybaker

Once you registered and staked tez, you need to wait the end of the current cycle plus ``CONSENSUS_RIGHTS_DELAY = 2`` cycles,
for your rights to be considered.

There is an RPC that can be used to check your rights for every
cycle, up to 2 cycles in the future.

::

   octez-client rpc get /chains/main/blocks/head/helpers/baking_rights\?cycle=300\&delegate=tz1_xxxxxxxxxxx\&max_round=2

Sometimes there is no consensus at a round, so it is worth considering also
baking rights at higher rounds, like 2 in the example above.

Configure the staking policy
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A delegate may choose to accept (or not) staked funds from external stakers.
Both the delegate's own stake and the stake from external stakers can be
:ref:`slashed<slashing>` (that is, partially lost), when the delegate misbehaves (see :ref:`accuser_run`).

Delegates can set two parameters by configuring their :ref:`staking policy <staking_policy_configuration>`:

- the maximum ratio of external stake over their own stake: a factor between 0 and 9, by default 0, which means that:

  + for any factor *f*, the delegate accepts *f* times its own stake from external stakers
  + by default, delegates don't allow external staking
- the proportion of rewards kept by the delegator (the rest being paid to external stakers): a factor between 0 and 1, by default 1.

These parameters are configured as follows::

  octez-client set delegate parameters for  <delegate> --limit-of-staking-over-baking <value> --edge-of-baking-over-staking <value>

If the delegated funds exceed 9 times the delegate’s own stake, the delegate is *overdelegated*. If the staked funds from external stakers exceed the proportion defined by the delegate, the delegate is *overstaked*.
See details and consequences in :ref:`staking_policy_configuration`.


.. _baker_run:

Baker
~~~~~

The baker is a daemon that executes Tezos' :doc:`consensus algorithm<../active/consensus>`.
The baker runs on behalf of one or more specified accounts or, if none is specified, on behalf of
all accounts whose secret keys are known.

To learn more about how the Octez baker works see :doc:`../shell/baker`.
Also, a complete manual page of the baker executable is available :ref:`here <baker_manual>`.

During its run, the baker bakes blocks (by selecting transactions from
the mempool and arranging them in a new block) and emits consensus
operations like attestations. It does so whenever the associated
accounts have the necessary rights.
In our case, we already set up and funded a baker account called "mybaker".

For its operation, the baker daemon needs to communicate with an Octez node using RPCs.
Therefore, you must indicate a node endpoint whose RPC interface is available.
The most common way is to :ref:`run a node locally <quickstart_node>`, ensuring its RPC port is open for local calls (e.g. ``--rpc-addr 127.0.0.1``).

Bakers are also supposed to run a DAL node before running the baker daemon, to expand the amount of data that Tezos can distribute without causing congestion on Layer 1.
Therefore, first :doc:`start a DAL node <../shell/dal_run>`.

Typically, you run an attester DAL node as follows::

    octez-dal-node run --endpoint <node-endpoint> --attester-profiles=<baker-addr>

Once the DAL node runs, we can launch the baker daemon pointing to the standard node directory and
baking for user "mybaker"::

   octez-baker run with local node ~/.tezos-node mybaker --liquidity-baking-toggle-vote pass

.. note::
	Previously, you had to run a specific baker for a given protocol, such as ``octez-baker-<PROTO_HASH>``,
	where ``PROTO_HASH`` is the short hash of the current protocol of the network you want to bake on.
	These protocol-specific bakers are still available for now, but will be deprecated.

Note that:

- the baker needs direct access to the node data directory for performance reasons (to reduce the number of RPC calls to the node).
- since version 13.0, option ``--liquidity-baking-toggle-vote`` is mandatory, see :ref:`the changelog <changes_13_0_rc1_baker>`.
- option ``--liquidity-baking-toggle-vote`` must be placed **after** ``run`` on the command-line.
- starting with version 22.0 it is recommended to run the Octez node with a DAL node (opting out requires the explicit option ``--without-dal``).
- starting with version 23.0, and the activation of protocol S, when running a baker with the DAL activated, and a tz4 consensus key, it is required to have also a tz4 :ref:`companion key <companion_key>` registered for the delegate to produce DAL attestations. Otherwise the baker will only produce regular attestations, without any DAL information, and the baker will be seen as not participating in the DAL.

Quickstart baker
~~~~~~~~~~~~~~~~

Putting together all the above instructions, you may want to quickly start a baker daemon on a testnet such as ghostnet as follows (after :ref:`starting an Octez node <quickstart_node>`)::

    octez-client gen keys mybaker
    BAKER_ADDRESS=$(octez-client show address mybaker | grep Hash | cut -d' ' -f2)
    # Fund BAKER_ADDRESS with > 6000 tez, e.g. at https://faucet.ghostnet.teztnets.com
    octez-client register key mybaker as delegate
    octez-client stake 6000 for mybaker
    octez-dal-node run --endpoint http://127.0.0.1:8732 --attester-profiles=$BAKER_ADDRESS
    octez-baker run with local node $HOME/.tezos-node mybaker --liquidity-baking-toggle-vote pass --dal-node http://127.0.0.1:10732


.. warning::

    **Remember that having two bakers running connected to the same account could lead to double baking/attesting and the loss of all your bonds.**
    If you are worried about the availability of your node when it is its turn to bake/attest, there are other ways than duplicating your credentials (see the discussion in section :ref:`inactive_delegates`).
    **Never** use the same account on two daemons.

However, if you are using the deprecated protocol-suffixed executables, it is safe (and actually necessary) to temporarily run two bakers just before a protocol activation: the baker for the protocol being replaced and the baker for the protocol to be activated.
This is no longer necessary with the unique ``octez-baker`` executable.


.. note::

   It is possible to bake and attest using a dedicated :ref:`consensus_key` instead of the delegate's key.

The baker uses the same format of configuration file as the client, and the default configuration file for the baker is the same as the default configuration file for the client (see :ref:`client_conf_file`).

.. warning::

    When running a baker, it is recommended to carefully save the nonces generated by the baker as part of the :doc:`consensus protocol <../active/consensus>`, to be able to reveal the nonces before the end of the cycle even if the baker is restarted (e.g., on another machine), so as to avoid losing :ref:`attestation rewards <slashing>`.

.. _inactive_delegates:

Inactive delegates
~~~~~~~~~~~~~~~~~~

If a delegate doesn't show any sign of activity for :ref:`TOLERATED_INACTIVITY_PERIOD <ps_constants>` cycles,
it is marked **inactive** and its rights are removed.
This mechanism is important to remove inactive delegates and reallocate
their rights to the active ones so that the network is always working
smoothly.
Normally even a baker with the minimal stake should perform enough
operations during this period to remain active.
If for some reason your delegate is marked inactive you can reactivate
it simply by re-registering again like above.

To avoid your Tezos delegate being marked inactive while pausing it for maintenance work, it is advised to check the schedule of future baking and attesting slots assigned to it, using a :ref:`Tezos block explorer <block_explorers>`.
Alternatively, you may use the baking rights RPC and the attesting rights RPC (see :doc:`../api/openapi`), which is able to return a list of baking/attesting slots for a given delegate (see :ref:`example <DelegateRegistration>`).


.. _accuser_run:

Accuser
~~~~~~~

The accuser is a daemon that monitors all blocks received on all
chains and looks for:

* bakers who signed two blocks at the same level and the same round
* bakers who injected more than one pre-attestations or attestation operation for the
  same level and round (more details :doc:`here <../active/consensus>`)

Upon finding such irregularity, it will emit respectively a
double-baking, double-pre-attesting, or double-attesting denunciation operation, which will
cause the offender to be :ref:`slashed<slashing>`, that is, to lose part of its security deposit.

::

   octez-accuser run

.. note::
	Previously, you had to run a specific accuser for a given protocol, such as ``octez-accuser-<PROTO_HASH>``,
	where ``PROTO_HASH`` is the short hash of the current protocol of the network you want to bake on.
	These protocol-specific accusers are still available for now, but will be deprecated.

The accuser uses the same format of configuration file as the client (see :ref:`client_conf_file`).
A complete manual page of the accuser is available :ref:`here <accuser_manual>`.


DAL node
~~~~~~~~

To add a Data Availability Layer (DAL) node to support data transmission across the network, see :doc:`DAL layer <../shell/dal_overview>`.

Docker
~~~~~~

If you are running the baker Docker image, you can watch the baker logs with
``docker logs``. First, find the name of your container with::

    docker ps

If your container is running, its name will appear in the last column.
For instance, if the name is ``mainnet_baker``, you can
view recent logs with::

    docker logs mainnet_baker

If you want to keep watching logs, use ``-f``::

    docker logs mainnet_baker -f

This allows you to know if you baked.
You should see lines such as::

    Injected block BLxzbB7PBW1axq for bootstrap5 after BLSrg4dXzL2aqq  (level 1381, slot 0, fitness 00::0000000000005441, operations 21)
