.. TODO tezos/tezos#2170: search shifted protocol name/number & adapt

How to run Tezos
================

In this section, we discuss how to take part in the protocol that runs
the network.
There are two main ways to participate: delegating
your coins and running a delegate.
The main advantage of delegating your coins is simplicity.
The second way allows to participate more actively in the protocol, by baking blocks and voting, but is more demanding; however, the extra effort is compensated by more rewards in tez.

To learn more about the protocol refer to :doc:`this page <../active/protocol_overview>`.

No matter how you decide to run Tezos, your node must have an accurate time source and be properly synchronized to it, e.g. by configuring an NTP daemon.
This is especially important for bakers, as baking nodes desynchronized from the correct time of day have caused operational problems in the past by "baking in the future".

.. _delegating_coins:

Delegating your coins
---------------------

If you don't want to deal with the complexity of running your own
delegate, you can always take part in the protocol by delegating your
coins to one.

Both implicit accounts and smart contracts can have a
delegate. Setting or resetting the delegate of an implicit account is
achieved by the following command:

::

   tezos-client set delegate for <implicit_account> to <delegate>

where ``<implicit_account>`` is the address or alias of the implicit
account to delegate and ``<delegate>`` is the address or alias of the
delegate (which has to be :ref:`registered<DelegateRegistration>`).

To stop a delegation, the following command can be used:

::

   tezos-client withdraw delegate from <implicit_account>



Smart contract can also delegate the tokens they hold to registered
delegates. The initial delegate of a smart contract can be set at
origination using the ``--delegate`` option:

::

    tezos-client originate contract <contract_alias> transferring <initial_balance> from <originator> running <script> --delegate <delegate> --burn-cap <cap>


Once the contract is originated, the only way to stop or modify its
delegation is by using the ``SET_DELEGATE`` Michelson instruction (see
:ref:`the Michelson documentation<MichelsonSetDelegate>` for more
details).


Notice that only implicit accounts can be delegates, so your delegate
must be a *tz* address.

Funds in implicit accounts which are not registered as delegates
do not participate in baking.

Note that delegating coins doesn't mean that a delegate can spend
them, they only add to its delegated balance.
In turn, delegators can freely spend their own funds in spite of the active delegation (they are not locked, like in other PoS algorithms).
Technically, delegation is a link between a delegator account and a delegate account, meaning that *all* the funds of the former are delegated to the latter, until the delegation is withdrawn.
When a delegator spends their tokens, the delegated balance of their delegate decreases; conversely, when they receive tokens the delegated balance of their delegate increases.


Running a delegate
------------------

A delegate is responsible for baking blocks, endorsing blocks and
accusing other delegates in case they try to double bake or double
endorse. A delegate is also responsible for taking part in the
:doc:`governance process<../active/voting>`.

Rights for baking and endorsing are randomly assigned
to delegates proportionally to their :ref:`active stake<active_stake>`,
which usually is the same as their staking balance,
that is, their own balance plus their delegated balance.

A minimum active stake of one roll
is required for participating in consensus and in governance.
A :ref:`roll<glossary_roll>` is just an amount of 6kꜩ and all
computations with rolls are rounded to the nearest lower integer
e.g. if you have 15kꜩ it amounts to 2 rolls. Rolls are used as a unit
to determine delegates' voting rights in governance, while rights in
consensus are not roll-based, they are based on the actual, non-approximated stake.

Delegates are required to freeze around 10% of their active stake into
a security deposit (more precisely, it's 10% of the maximum active
stake during the last 7 cycles). A delegate is
:ref:`slashed<slashing_ithaca>`, that is, it looses funds from its
security deposits when it misbehaves by double-signing. The funds in
the security deposit come from the delegate's account. In case a
delegate is over-delegated (that is, its own balance does not cover
10% of its staking balance), the delegate's active balance is then set
to be 10 times its own balance. Delegates can set an upper limit to their
frozen deposits with the following command:

::

   tezos-client set deposits limit for <delegate> to <limit>


On testnets, when you obtain coins from :ref:`the faucet<faucet>`, if you
are lucky to obtain more than one roll, you can register a delegate
using this identity.
Otherwise, you need to ask the faucet for more accounts and
delegate them to the first.


.. _DelegateRegistration:

Register and check your rights
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To run a delegate, you first need to register as one using
your implicit account::

   tezos-client register key bob as delegate

Once registered, you need to wait ``preserved_cycles + 2 = 7`` cycles
for your rights to be considered.

There is a simple rpc that can be used to check your rights for every
cycle, up to 5 cycles in the future.

::

   tezos-client rpc get /chains/main/blocks/head/helpers/baking_rights\?cycle=300\&delegate=tz1_xxxxxxxxxxx\&max_round=2

Sometimes there is no consensus at a round, so it is worth considering also
baking rights at higher rounds, like 2 in the example above.

.. _inactive_delegates:

Inactive delegates
~~~~~~~~~~~~~~~~~~

If a delegate doesn't show any sign of activity for `preserved_cycles`
it is marked **inactive** and its rights are removed.
This mechanism is important to remove inactive delegates and reallocate
their rights to the active ones so that the network is always working
smoothly.
Normally even a baker with the minimal stake should perform enough
operations during 5 cycles to remain active.
If for some reason your delegate is marked inactive you can reactivate
it simply by re-registering again like above.

To avoid your Tezos delegate being marked inactive while pausing it for maintenance work, it is advised to check the schedule of future baking and endorsing slots assigned to it, using a block explorer in the :ref:`Tezos community <tezos_community>`.
Alternatively, you may use the baking rights RPC and the endorsing rights RPC (see :doc:`../api/openapi`), which is able to return a list of baking/endorsing slots for a given delegate (see :ref:`example <DelegateRegistration>`).

.. _baker_run:

Baker
~~~~~

The baker is a daemon that executes Tezos' :ref:<consensus algorithm<active/consensus>.
The baker runs on behalf of one or more specified accounts or, if none is specified, on behalf of
all accounts whose secret keys are known.

During its run, the baker bakes blocks (by selecting transactions from
the mempool and arranging them in a new block) and emits consensus
operations like endorsements. It does so whenever the associated
accounts have the necessary rights.

Let's launch the daemon pointing to the standard node directory and
baking for user *bob*::

   tezos-baker-alpha run with local node ~/.tezos-node bob

Note that the baker needs direct access to
the node data directory for performance reasons (to reduce the number of RPC calls to the node).

.. warning::

    **Remember that having two bakers running connected to the same account could lead to double baking/endorsing and the loss of all your bonds.**
    If you are worried about the availability of your node when it is its turn to bake/endorse, there are other ways than duplicating your credentials (see the discussion in section :ref:`inactive_delegates`).
    **Never** use the same account on two daemons.


.. note::

   In protocols before Ithaca, a separate daemon, the endorser, is responsible for emitting endorsements.
   In these protocols, one needs to run the daemon ``tezos-endorser-NNN-*`` to endorse.

Accuser
~~~~~~~

The accuser is a daemon that monitors all blocks received on all
chains and looks for:

* bakers who signed two blocks at the same level and the same round
* bakers who injected more than one pre-endorsements or endorsement operation for the
  same level and round (more details :doc:`here <../active/consensus>`)

Upon finding such irregularity, it will emit respectively a
double-baking, double-pre-endorsing, or double-endorsing denunciation operation, which will
cause the offender to be :ref:`slashed<slashing>`, that is, to lose part of its security deposit. 

::

   tezos-accuser-alpha run


Docker
~~~~~~

If you are running the baker Docker image, you can watch the baker logs with
``docker logs``. First, find the name of your container with::

    docker ps

If your container is running, its name will appear in the last column.
For instance, if the name is ``mainnet_baker-012-Psithaca_1``, you can
view recent logs with::

    docker logs mainnet_baker-012-Psithaca_1

If you want to keep watching logs, use ``-f``::

    docker logs mainnet_baker-012-Psithaca_1 -f

This allows you to know if you baked.
You should see lines such as::

    Injected block BLxzbB7PBW1axq for bootstrap5 after BLSrg4dXzL2aqq  (level 1381, slot 0, fitness 00::0000000000005441, operations 21)
