.. TODO tezos/tezos#2170: search shifted protocol name/number & adapt

How to run Tezos
================

In this section, we discuss how to take part in the protocol that runs
the network.
There are two main ways to participate in the consensus: delegating
your coins and running a delegate.
The main advantage of delegating your coins is simplicity.
The second way allows to participate more actively in the protocol, by baking blocks and voting, but is more demanding; however, the extra effort is compensated by more rewards in tez.

To learn more about the protocol refer to :doc:`this page <../active/proof_of_stake>`.

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


Running a delegate
------------------

A delegate is responsible for baking blocks, endorsing blocks and
accusing other delegates in case they try to double bake or double
endorse.

In the network, rights for baking and endorsing are randomly assigned
to delegates proportionally to the number of rolls they have been
delegated.
A roll is just a block of 8kꜩ and all computations with rolls are
rounded to the nearest lower integer e.g. if you have 15kꜩ it amounts
to 1 roll.

When you obtain coins from :ref:`the faucet<faucet>`, if you
are lucky to obtain more than one roll, you can register a delegate
using this identity.
Otherwise, you need to ask the faucet for more accounts and
delegate them to the first.

.. _over_delegation:

Deposits and over-delegation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When baking or endorsing a block, a *security deposit* (or *bond*) is
frozen for ``preserved_cycles`` cycles from the account of the
delegate.
Hence a delegate must have enough funds to be able to pay security
deposits for all the blocks it can potentially bake/endorse during
``preserved_cycles``.
The current deposits are *640ꜩ* for baked block and *2.5ꜩ* for
endorsement.
Note that delegating coins doesn't mean that a delegate can spend
them, they only add up to its rolls count while all the deposits must
come from the delegate's account.
In turn, delegators can freely spend their own funds in spite of the active delegation (they are not locked, like in other PoS algorithms).
Technically, delegation is a link between a delegator account and a delegate account, meaning that *all* the funds of the former are delegated to the latter, until the delegation is withdrawn.
When a delegator spends their tokens, the delegated balance of their delegate decreases; when they receive tokens the delegated balance of their delegate increases.
If a delegate runs out of funds to deposit it won't be able to bake or
endorse. Other than being a missed opportunity for them, this has also
negative consequences on the network.
Missing baking or endorsing slots slows down the network, as it is necessary to wait some time for the baker at the next priority to bake, and also some other time for each missed endorsing slot.
Besides, missed endorsements also makes the chain more susceptible to forks.
Running out of funds can happen if a delegate is *over-delegated*,
that is if the amount of rolls it was delegate is disproportionate
with respect to its available funds.

It is in the interest of every delegator to make sure a delegate is
not already over-delegated, because a delegate cannot refuse a delegation.
Indeed, over-delegation translates in missed baking and endorsing slots, as explained above, and hence in missed rewards. On the other hand,
each delegate should plan carefully its deposits, as explained next, by buying more tez if needed.

.. _expected_rights:

Expected rights, deposits and rewards
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's assume we have 1 roll, we want to estimate our chances to bake
or endorse to prepare the funds for our deposits.
Our chances depend on how many rolls are currently active in the
network, once we know that we can estimate how many blocks and
endorsements we could be assigned in a cycle.
The number of active rolls can be computed with two RPCs. First, we
list all the active delegates with ``delegates?active``. Then, we sum
all their ``stacking_balance``. Finally, we simply divide by the size of a
roll, 8kꜩ.
For example, if the number of active rolls is ~30k then,
for each block, we know that the chance that we get selected for
baking is ``1/30k`` while for endorsing is 32 times that.
Given that every draw is with replacement, the distribution that
describes our chances of being selected is the binomial with
probability of success ``p=1/30k``.
The distribution has another parameter ``n`` for the number of times
we draw, in our case in a cycle the draws for baking are ``n_b =
4096`` while for endorsing are ``n_e = 4096 * 32``.
Moreover we could extend ``n`` to cover ``preserved_cycles = 5``.
Once we have ``p`` and ``n``, the expected number of times that we
might get selected is ``p * n`` (the mean of the distribution).
Over many cycles, our chances will fall around the mean, in some cycles,
we might get unlucky and be assigned fewer rights, but in some cycles we might
get lucky and be assigned more rights!
Clearly, we would like to plan and have enough deposits to cover
also the "lucky" cycles so we need to compute a sort of "maximum"
number of rights that is safe for `most cases`.
We can compute this maximum using the inverse of Cumulative
Distribution Function of the Binomial where `most cases` is a value of
confidence that we can put to 95%.
There a simple `Python
script <https://gitlab.com/paracetamolo/utils/blob/master/estimated-rights.py>`_
that does the computation for us and returns the deposits and rewards,
expected and maximum, for a cycle and ``preserved_cycles``.

::

   prob success 3.333333e-05
   confidence   0.95
   ----------one-cycle--------------------
   blocks
    mean 0.14
    max  1.00
   endorsements
    mean 4.37
    max  8.00
   deposits
    mean 69.91 + 279.62
    max  512.00 + 512.00
   rewards
    mean 2.18 + 8.74
    max  16.00 + 16.00
   ----------preserved-cycles-------------
   blocks
    mean 0.68
    max  2.00
   endorsements
    mean 21.85
    max  30.00
   deposits
    mean 349.53 + 1398.10
    max  1024.00 + 1920.00
   rewards
    mean 10.92 + 43.69
    max  32.00 + 60.00

As a rule of thumb if we want to have very high confidence that we
won't miss any opportunity we should have around ~3kꜩ for deposits,
on the other hand, the expected returns will probably be around ~10ꜩ per cycle.

After ``preserved_cycles``, not only does the delegate take back control of
its frozen deposits, but it also receives its rewards for baking and endorsing.
Additionally, a baker also receives the fees of the operations it
included in its blocks.
While fees are unfrozen after ``preserved_cycles`` like deposits and
rewards, they participate in the staking balance of the delegate
immediately after the block has been baked.


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

   tezos-client rpc get /chains/main/blocks/head/helpers/baking_rights\?cycle=300\&delegate=tz1_xxxxxxxxxxx\&max_priority=2

Sometimes a delegate skips its turn so it is worth considering also
baking rights at priority 2 like in the example above.
There is no priority for endorsements, every missed endorsement is
lost.

.. _inactive_delegates:

Inactive delegates
~~~~~~~~~~~~~~~~~~

If a delegate doesn't show any sign of activity for `preserved_cycles`
it is marked **inactive** and its rights are removed.
This mechanism is important to remove inactive delegates and reallocate
their rights to the active ones so that the network is always working
smoothly.
Normally even a baker with one single roll should perform enough
operations during 5 cycles to remain active.
If for some reason your delegate is marked inactive you can reactivate
it simply by re-registering again like above.

To avoid your Tezos delegate being marked inactive while pausing it for maintenance work, it is advised to check the schedule of future baking and endorsing slots assigned to it, using a block explorer in the :ref:`Tezos community <tezos_community>`.
Alternatively, you may use the baking rights RPC and the endorsing rights RPC (see :doc:`../api/openapi`), which is able to return a list of baking/endorsing slots for a given delegate (see :ref:`example <DelegateRegistration>`).

.. _baker_run:

Baker
~~~~~

The baker is a daemon that, once connected to an account, computes the
baking rights for that account, selects transactions from the mempool
and bakes blocks.
Note that the baker needs direct access to
the node data directory for performance reasons (to avoid RPC calls to the node).

Let's launch the daemon pointing to the standard node directory and
baking for user *bob*::

   tezos-baker-alpha run with local node ~/.tezos-node bob

.. warning::

    **Remember that having two bakers or endorsers running connected to the same account could lead to double baking/endorsing and the loss of all your bonds.**
    If you are worried about the availability of your node when it is its turn to bake/endorse, there are other ways than duplicating your credentials (see the discussion in section :ref:`inactive_delegates`).
    **Never** use the same account on two daemons.

Endorser
~~~~~~~~

The endorser is a daemon that, once connected to an account, computes
the endorsing rights for that account and, upon reception of a new
block, verifies the validity of the block and emits an endorsement
operation.
It can endorse for a specific account or if omitted it endorses for
all known accounts.

.. note::

   In the Alpha protocol, the endorser daemon no longer exists, its role being
   played by the baker daemon in the corresponding consensus algorithm.
   Therefore, there is no ``tezos-endorser-alpha`` executable, but endorser
   executables ``tezos-endorser-NNN-*`` exist for protocols up to 011
   (Hangzhou).

Accuser
~~~~~~~

The accuser is a daemon that monitors all blocks received on all
chains and looks for:

* bakers who signed two blocks at the same level
* endorsers who injected more than one endorsement operation for the
  same baking slot (more details :doc:`here <../active/proof_of_stake>`)

Upon finding such irregularity, it will emit respectively a
double-baking or double-endorsing denunciation operation, which will
cause the offender to lose its security deposit.

::

   tezos-accuser-alpha run


Docker
~~~~~~

The docker image runs the daemons by default for all your keys.
Assuming you run on Hangzhounet, to know if you baked, just run::

    ./hangzhounet.sh baker log
    ./hangzhounet.sh endorser log

(replace ``hangzhounet.sh`` with ``mainnet.sh`` for Mainnet).
You should see lines such as::

    Injected block BLxzbB7PBW1axq for bootstrap5 after BLSrg4dXzL2aqq  (level 1381, slot 0, fitness 00::0000000000005441, operations 21)

Or::

    Injected endorsement for block 'BLSrg4dXzL2aqq'  (level 1381, slot 3, contract bootstrap5) 'oo524wKiEWBoPD'
