The consensus algorithm
=======================

This document provides a high-level description of Tenderbake, the Tezos
:doc:`proof-of-stake<proof_of_stake>` consensus algorithm.

History
-------

Before Tenderbake, there was
`Emmy* <https://gitlab.com/tezos/tzip/-/blob/1728fcfe0ac90463ef15e6a994b6d6a15357e373/drafts/current/draft_emmy-star.md>`_,
a Nakamoto-style consensus consisting of a series of improvements of the one in
the `Tezos whitepaper <https://tezos.com/whitepaper.pdf>`_.

Emmy*, like any Nakamoto-style consensus algorithm (such as `Bitcoin
<https://bitcoin.org/bitcoin.pdf>`_ or `Ouroboros
<https://eprint.iacr.org/2016/889>`_), offers *probabilistic*
finality: forks of arbitrary length are possible but they collapse
with a probability that increases rapidly with fork length.

`Tenderbake <https://arxiv.org/abs/2001.11965>`_ instead, like any classic
BFT-style consensus algorithm (such as
`PBFT <https://pmg.csail.mit.edu/papers/osdi99.pdf>`_ or
`Tendermint <https://arxiv.org/abs/1807.04938>`_), offers *deterministic*
finality: a block that has just been appended to the chain of some node is known
to be final once it has two additional blocks on top of it, regardless of
network latency.


Overview
--------

The starting point for Tenderbake is
`Tendermint <https://arxiv.org/abs/1807.04938>`_, the first classic-style algorithm
for blockchains.

Tenderbake adapts Tendermint to the Tezos blockchain, but the adjustments
required are
`substantive <https://research-development.nomadic-labs.com/a-look-ahead-to-tenderbake.html#the-tezos-architecture>`_:

* Tenderbake is tailored to match the Tezos architecture by using only
  communication primitives and network assumptions which Tezos supports.
* Tenderbake makes weaker network assumptions than Tendermint, at the price of
  adding the extra assumption that participants have loosely synchronized clocks
  — which is fine, because Tezos already uses them.

The design of Tenderbake and its rationale are described at
length in the `technical report <https://arxiv.org/abs/2001.11965>`_ and in a
`Nomadic Labs's blog
post <https://research-development.nomadic-labs.com/a-look-ahead-to-tenderbake.html>`_. Here we
only provide a user/developer perspective.

.. _tb_validator_beta:

Tenderbake is executed for each new block level by a "committee" whose members
are called *validators*, which are delegates selected at random based on their
stake, in the same way as endorsers were selected in Emmy*. We let
``CONSENSUS_COMMITTEE_SIZE`` be the number of validator :ref:`slots<rights_beta>` per level.
Furthermore, we use ``CONSENSUS_THRESHOLD`` to denote two thirds of ``CONSENSUS_COMMITTEE_SIZE``.

For each level, Tenderbake proceeds in rounds. Each *round* represents an
attempt by the validators to agree on the content of the block for the current
level, that is, on the sequence of non-consensus operations the block contains.
We call this sequence the block's *payload*.

Each round has an associated duration. Round durations are set to increase so
that for any possible message delay, there is a round that is sufficiently long
for all required messages to be exchanged.
Round durations depend on protocol parameters ``MINIMAL_BLOCK_DELAY`` and ``DELAY_INCREMENT_PER_ROUND``.
These parameters specify round durations as follows:

.. math::

     round\_duration(0) &= minimal\_block\_delay \\
     round\_duration(r+1) &= round\_duration(r) + delay\_increment\_per\_round \\
     & = minimal\_block\_delay + (r + 1) * delay\_increment\_per\_round

Round durations thus increase linearly with ``DELAY_INCREMENT_PER_ROUND``.

Schematically, a round consists in the following steps:

.. _candidate_block_beta:

* a validator designated for that round injects a *candidate block* (representing a proposal) and consensus operations (representing votes) into the node to which it is attached, which then
* diffuses those blocks and consensus operations to other nodes of the network, and thus
* communicates them to the validators attached to those nodes, to carry out voting on which block to accept.

.. _quorum_beta:

Unlike Emmy*, Tenderbake has `two types of
votes <https://research-development.nomadic-labs.com/a-look-ahead-to-tenderbake.html#why-do-we-need-preendorsements>`_:
before attesting a block ``b``, a validator preattests ``b``. Furthermore,
to be able to attest, a validator must have observed a preattestation *quorum*, that is a
set of preattestations from validators having at least ``CONSENSUS_THRESHOLD`` validator slots. Similarly, to be able to decide, a validator must have observed an attestation quorum, that is, a set of attestations from validators having at least ``CONSENSUS_THRESHOLD`` validator slots. The
attestation quorum for a block ``b`` is included in a block ``b'`` on top of ``b``,
serving as a certification that ``b`` has been agreed upon.
We also say that block ``b'`` confirms block ``b``.

The validator's whose turn is to inject a candidate block at a given round is
called the *proposer* at that round. Proposers in Tenderbake are selected
similarly to bakers in Emmy*: the proposer at round ``r`` is the
validator who has the validator slot ``r``. A proposer who has observed a
preattestation quorum for a candidate block in a previous round, is required to propose a block with
the same *payload* as
the initial block. We talk about a *re-proposal* in this case.


.. _finality_beta:

Transaction and block finality
------------------------------

A transaction is final as soon as the block including it has a confirmation (that is, a block on top of it).
Indeed, as hinted above, a block contains the certification (that is, the attestation quorum) for the previous
payload. Thanks to the attestation quorum, Tenderbake guarantees **transaction finality
after 1 confirmation**.

It may be possible that different validators decide at different rounds, though on the same payload. The blocks at these different rounds differ precisely because they contain, in the header, as part of the block fitness,
the round at which they were proposed.
Among these "candidate" blocks, the block with the smallest round has the highest fitness and so it will be the one decided.
Consequently, to agree on a block, that is, on both the payload and the header, Tenderbake needs one more
confirmation, and thus guarantees
**block finality after 2 confirmations**.

Time between blocks
-------------------

The time between blocks represents the difference between the timestamps of the blocks. The timestamp of a block is given by the beginning of the round at which the block has been agreed upon. Thus, the time between blocks depends on the round at which decisions are taken. For
example, if the decision at the previous level was taken at round 4 and at the current level at round 2, then the current block's delay relative to
its predecessor, is :math:`round\_duration(4) + round\_duration(0) + round\_duration(1)`.
The general case is as follows, say that the decision at the previous
level is taken at round ``m`` and the decision at the current level is
taken at round ``n``, then the current block's delay relative to its
predecessor is :math:`round\_duration(m) + \sum_{i=0}^{n-1} round\_duration(i)`.
We note that, under
normal network conditions, and with active and compliant validators, decisions
should be taken at round 0, meaning that the time between blocks would be
:math:`round\_duration(0)` seconds i.e., parameter ``MINIMAL_BLOCK_DELAY``.


.. _active_stake_beta:

Validator selection: staked balance and active stake
----------------------------------------------------

Validator selection is based on the staked balance of a delegate, as in Emmy*.
Let us first (re)define these and related concepts.

- The *overall balance* of a delegate is its full balance (i.e. all the tokens owned by the delegate) plus the
  balances of all accounts that have delegated to it.
  It must be at least ``MINIMAL_STAKE`` tez, otherwise the delegate cannot be selected as a validator.
- The *active stake* of a delegate is the amount of tez with which
  it participates in consensus. It is at most its maximal
  staked balance. We explain below how it is computed.
- The *staked balance* represents the delegate's skin in the game: in
  the case that the delegate behaves badly, its staked balance is
  partly :ref:`slashed<slashing_beta>`. This staked balance must be
  at least ``MINIMAL_FROZEN_STAKE`` tez, otherwise the delegate cannot
  be selected as a validator. Note that until the :ref:`activation of
  Adaptive Issuance and Staking<feature_activation_beta>`, the
  staked balance is automatically updated at the end of each cycle to
  maximize the active stake.
- The *spendable balance* of a delegate is its full balance
  minus its staked balance and unstaked frozen balance.

We state next the RPCs which allow to retrieve these types of balances, and also some invariants about them
(Note that these are just invariants, not definitions; for
instance, the frozen deposits are computed in terms of the full balance,
not the other way around.):

- ``delegated balance`` represents the total amount of tokens delegated or staked by others to a
  given delegate; it excludes the delegate's full balance; it is obtained
  with ``../context/delegates/<pkh>/delegated_balance``
- ``overall balance = full balance + delegated balance``; it is obtained with
  ``../context/delegates/<pkh>/staking_balance``
- ``full balance = spendable balance + staked balance + unstaked frozen balance``; it is obtained with
  ``../context/delegates/<pkh>/full_balance``
- ``staked balance`` is obtained with ``../context/delegates/<pkh>/frozen_deposits``
- ``spendable balance`` is obtained with ``../context/contracts/<pkh>/balance``

Until Adaptive Issuance, delegates can set an upper limit to their staked balance with the
command ``octez-client set deposits limit for <delegate> to
<deposit_limit>``, and unset this limit with the command ``octez-client
unset deposits limit for <delegate>``. These commands are implemented
using the manager operation ``Set_deposits_limit``.
When emitting such a command in cycle ``c``, it affects the automatic deposit at
the end of this cycle, and thus the consensus rights set for cycle ``(c + 1) +
CONSENSUS_RIGHTS_DELAY + 1``.
Since the deposit will be adjusted at the end of cycle ``c``, unstaked tokens
will be available at cycle  ``c + 1 + CONSENSUS_RIGHTS_DELAY + MAX_SLASHING_PERIOD``.

The active stake is computed ``CONSENSUS_RIGHTS_DELAY`` in advance: at
the end of cycle ``c`` for cycle ``c + 1 + CONSENSUS_RIGHTS_DELAY`` (as in Emmy*),
before updating the delegates' :ref:`activity status<active_delegate_beta>`.

..
   This entails that a delegate which was participating until cycle ``c -
   1`` and is no longer participating in cycle ``c`,
   will lose its rights from cycle
   ``c + 2 * CONSENSUS_RIGHTS_DELAY + 2`` onwards -- at the end of cycle ``c +
   CONSENSUS_RIGHTS_DELAY``, the rights for cycle ``c + 2 *
   CONSENSUS_RIGHTS_DELAY + 1`` are computed, and only then is the delegate
   declared passive. Here "participation" means *having baked a final
   block* or *having a preattestation or attestation included in a final
   block*.

Intuitively, the active stake is set to 10 times the delegate's staked balance,
without going beyond its overall balance.
More precisely, the active stake is:

- the delegate's staked balance,
- its stakers' staked balance (up to a limit, see
  :ref:`limit_of_staking_over_baking<staking_policy_configuration_beta>`),
- and the liquid delegated balance + the spendable balance, up to 9 times the delegate's staked balance.

Before Adaptive Issuance, each part weighs equally when computing the baking and voting rights. After Adaptive Issuance, the frozen balances (non-liquid, non-spendable) are weighed for twice as much per tez as the liquid part.
Moreover:

- Rights originating from staked tez are computed from the value at the end of the cycle;
- Rights originating from delegated tez are computing using the minimum value over the cycle.

Let's take some examples. Say that the full balance of a delegate is ``1000`` tez.
Then, without external staking, its theoretical maximum active stake is
``10000`` tez. The following table lists some scenarios before Adaptive Issuance (assuming for
simplicity no changes in the delegate's full and staked balances
during the last 5 cycles).

.. list-table::
   :widths: 20 20 20 20 20
   :header-rows: 1

   * - Overall balance
     - Frozen deposit limit
     - Active stake
     - Staked balance
     - Spendable balance
   * - 9000
     - --
     - 9000
     - 900
     - 100
   * - 12000
     - --
     - 10000
     - 1000
     - 0
   * - 9000
     - 400
     - 4000
     - 400
     - 600
   * - 12000
     - 400
     - 4000
     - 400
     - 600

We note in passing that this new schema basically solves the main
problem of over-delegation: a delegate will not fail anymore to bake
and attest because of an insufficient balance to pay the
deposit. However, a delegate can still be over-delegated, and it will be
rewarded based on its active stake, not on its overall balance.


Delegated balance used for baking rights
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The delegated balance used for computing baking rights is the minimum
of the total delegated amount over the whole cycle. It prevents any
manipulation of rights through short-duration transfers.

In the Paris protocol, the considered minimum is the minimum at any
point during block applications, which can be reached in the middle of
executing a transaction.

For example, if a baker transfers tez to one of its delegators, this
is internally treated as first removing the transferred amount from
the total amount delegated to this baker, then adding it back. In
between executing both updates, the total delegated amount is lower so
it might be the new minimum over the whole cycle. In other words, the
transferred tez risk not counting towards the baking rights that will
be computed at the end of the cycle, even though they have been owned
by an account delegating to this baker during the whole cycle.

Besides, when the minimum is reached in the middle of a block's
operations, the context for this minimum is not directly accessible
via RPC. In that case, in order to retrieve this exact context, one
needs to replay the block's balance updates on their own.

In the Quebec protocol, to solve these problems, only the total
delegated amounts **at the end of blocks** count when determining this
minimum. This is known as the **per-block min-delegated
feature**. This solution no longer penalizes baking rights when large
amounts are transferred between two accounts delegated to the same
baker. Moreover, it lets users easily retrieve via RPC the exact
context that the minimum comes from, since it is guaranteed to
correspond to the end of a block.

The minimum delegated in the current cycle can be retrieved with RPC
``GET
'/chains/<chain_id>/blocks/<block_id>/context/delegates/<delegate_id>/min_delegated_in_current_cycle'``.
This RPC returns the minimal value of the baker’s total delegated
amount at the end of a block, from the first block of the cycle up to
the block on which the RPC is called. It also returns the earliest
level at the end of which this minimum has been reached in the current
cycle.


Example
"""""""

At level ``150``, the baker receives ``50`` tez from a contract that
is not a delegator for this baker. Then, at level ``200``, the baker
transfers ``150`` tez to one of its delegators. Finally, at level
``205``, that delegator sends ``70`` tez to another contract that is
not a delegator for this baker.

The first transfer of ``50`` tez increases the baker’s ``total_delegated``
by ``50``. Then, the transfer of ``150`` tez is internally implemented
as removing ``150`` tez from the total delegated of the sender’s
delegate -- which is the baker itself, then adding ``150`` tez to the
``total_delegated`` of the destination’s delegate -- which is the same
baker in our case. Finally, the ``70``-tez transfer just removes
``70`` tez from the ``total_delegated`` of the same baker again.

Let's say that ``blocks_per_cycle = 128`` (as on Parisnet), so the
first level of the current cycle is ``129``, and let's say that
``total_delegated`` was ``1000`` at the beginning of the cycle.

.. list-table::
   :widths: 14 16 14 14 14 14 14
   :header-rows: 1

   * -
     -
     - L129: first level of cycle
     - L150: add 50
     - L200: remove 150
     - L200: add 150
     - L205: remove 70
   * -
     - Baker's current ``total_delegated`` (tez)
     - 1000
     - 1050
     - 900
     - 1050
     - 980
   * - Quebec RPC
     - Returned min (tez)
     - 1000
     - 1000
     -
     - 1000
     - 980
   * - Quebec RPC
     - Returned level
     - 129
     - 129
     -
     - 129
     - 205
   * - Paris RPC
     - Returned min (tez)
     - 1000
     - 1000
     -
     - 900
     - 900
   * - Paris RPC
     - Returned level
     - None
     - 150
     -
     - 200
     - 200

Note there are empty cells in the table as RPCs cannot be called in
the middle of the block application. Also, the
``min_delegated_in_current_cycle`` RPC returns the value in mutez, but
here we use tez for simplicity.

* In Quebec:

  - At levels ``129`` and ``150``, the earliest level at the end of
    which the ``total_delegated`` is equal to the minimum ``1000`` is
    the first level of the cycle, that is, level ``129``.

  - At level ``200``, the ``900`` value happens in the middle of the
    block application so it is not considered. The new end-of-block
    value ``1050`` is not lower than the old minimum of ``1000``, so
    ``min_delegated_in_current_cycle`` stays at (min: ``1000``, level:
    ``129``).

  - At level ``205``, the new end-of-block value ``980`` is lower than
    the old minimum of ``1000``, so ``min_delegated_in_current_cycle``
    becomes (min: ``980``, level: ``205``).

* In Paris:

  - At level ``129``, the ``total_delegated`` has not changed since
    the start of the cycle. The Paris RPC returns level ``None`` in
    this case.

  - At level ``150``, the ``total_delegated`` has changed since the
    start of the cycle, but the minimum is actually the initial value
    it had at the start of the cycle. In this case, Paris RPC returns
    the earliest level at which the ``total_delegated`` has changed,
    that is, level ``150``.

  - At level ``200``, the ``total_delegated`` reaches a new minimum
    ``900``. Indeed, the Paris protocol does consider the values in
    the middle of the block application, so
    ``min_delegated_in_current_cycle`` becomes (min: ``900``, level:
    ``200``).

  - At level ``205``, the new value ``980`` is higher than the old
    minimum of ``900``, so ``min_delegated_in_current_cycle`` is still
    (min: ``900``, level: ``200``).


Economic Incentives
-------------------

As Emmy*, Tenderbake rewards participation in consensus and punishes bad
behavior. Notable changes however are as follows:

* Fees and baking rewards go to the payload producer, the one who selected the
  transactions to be included in the block (and was the first to propose a
  block with that payload). In case of re-proposal, the payload producer might
  be different from the block proposer, the baker who injects the block.
* Including extra attestations, that is, more than the minimal required to
  obtain a quorum, is rewarded with a bonus.
* Attesting rewards are shared equally among all validators. Participation above
  a minimal threshold per cycle is however required.
* Validators are rewarded instantaneously for baking blocks and including extra attestations, and not at the end of the cycle like in Emmy*.
* At the end of a cycle ``c``, the following actions happen:

  - the distribution of attesting rewards,
  - the selection of the consensus committee cycle ``c + CONSENSUS_RIGHTS_DELAY``, based on the current active stake distribution.


Fees
^^^^

The fees associated to the transactions included in a block go to the payload
producer. This is only natural given that this is the validator that selects the
transactions to be included; see `an in-depth blog
post <https://ex.rs/protocol-level-fees/>`_ for further motivation.

The payload producer is usually the same delegate as the block
proposer (that is, the one that signs and injects the block): that's
always true for blocks at round 0; however, in case of re-proposals
this is not necessarily the case (see the algorithm description above).

Fees are given to the payload producer immediately, that is, they are
already reflected in the blockchain state obtained after applying the injected
block.

Rewards
^^^^^^^

There are three kinds of rewards: baking rewards, attesting rewards, and a bonus for including extra attestations.

The baking rewards are treated in the same way as fees: they go to the *payload*
producer and are distributed immediately.

To encourage fairness and participation, the *block* proposer receives
a bonus for the extra attestations it includes in the block.
The bonus is proportional to the number of
validator slots above the threshold of ``CONSENSUS_COMMITTEE_SIZE * 2 / 3`` that
the included attestations represent. The bonus is also distributed
immediately.

The attesting rewards are distributed at the end of the cycle.
The attesting reward may be received even if not all of the validator's attestations are included in a block and is proportional to the validator's active stake (in other words, to its *expected* number of validator slots, and not its actual number of slots).
However, two conditions must be met:

- the validator has revealed its nonce, and
- the validator has been present during the cycle.

Not giving rewards in case of missing revelations is not new as it is :ref:`adapted<random_seed_beta>`
from Emmy*.
The second condition is new. We say that a delegate is *present* during a cycle
if the attesting power (that is, the number of validator slots at the
corresponding level) of all the attestations included by the delegate during the
cycle represents at least ``MINIMAL_PARTICIPATION_RATIO`` of the delegate's expected number of
validator slots for the current cycle (which is ``BLOCKS_PER_CYCLE *
CONSENSUS_COMMITTEE_SIZE * active_stake / total_active_stake``).

Regarding the concrete values for rewards, before Adaptive Issuance, we first fix the total reward per
level, call it ``total_rewards``, to ``80 / blocks_per_minute`` tez.
Assuming ``blocks_per_minute = 7.5``, ``total_rewards`` is 10.67 tez. With Adaptive Issuance, this value changes dynamically over time but for the sake of example, we will assume that the reward value stays the same as above.
We define:

- ``BAKING_REWARD_FIXED_PORTION := baking_reward_ratio * total_rewards``
- ``bonus := (1 - baking_reward_ratio) * bonus_ratio * total_rewards`` is the max bonus
- ``attesting_reward := (1 - baking_reward_ratio) * (1 - bonus_ratio) * total_rewards``

where:

- ``baking_reward_ratio`` to ``1 / 4``,
- ``bonus_ratio`` to ``1 / 3``.

Thus, we obtain ``BAKING_REWARD_FIXED_PORTION = 2.67`` tez,
(maximum) ``bonus = 2.67`` tez, and ``attesting_reward = 5.33`` tez.
The bonus per additional attestation slot is in turn ``bonus /
(CONSENSUS_COMMITTEE_SIZE / 3)`` (because there are at most
``CONSENSUS_COMMITTEE_SIZE / 3`` validator slots corresponding to the
additional attestations included in a block). The rewards per
attestation slot are ``attesting_reward / CONSENSUS_COMMITTEE_SIZE``.
Assuming ``CONSENSUS_COMMITTEE_SIZE = 7000``, we obtain a bonus per slot of
``2.67 / (7000 / 3) = 0.001143`` tez and an attesting
rewards per slot of ``5.33 / 7000 = 0.000761`` tez.

Let's take an example. Say a block has round 1, is proposed by
delegate B, and contains the payload from round 0 produced by delegate
A. Also, B includes attestations with attesting power ``5251``. Then A receives
the fees and 10 tez (the ``BAKING_REWARD_FIXED_PORTION``) as a reward for
producing the block's payload. Concerning the bonus, given that
``CONSENSUS_COMMITTEE_SIZE = 7000``, the minimum required validator slots is ``4667``, and there are ``2333 = 7000 - 4667`` additional validator slots.
Therefore B receives the bonus ``(5251 - 4667) * 0.001143 = 0.667512`` tez. (Note
that B only included attestations corresponding to ``584 = 5251 - 4667`` additional validator slots, about a quarter of the
maximum ``2333`` extra attestations it could have theoretically included.) Finally, consider some
delegate C, whose active stake at some cycle is 1% of the total stake. Note that
his expected number of validator slots for that cycle is
``1/100 * BLOCKS_PER_CYCLE * CONSENSUS_COMMITTEE_SIZE = 1/100 * 30720 * 7000 = 2,150,400``
slots. Assume also that the attesting power of C's attestations
included during that cycle has been ``1,987,456`` slots. Given that this number is
bigger than the minimum required (``2,150,400 * 2 / 3``), it receives an attesting
reward of ``2,150,400 * 0.000761 = 1636.4544`` tez for that cycle.

.. _slashing_beta:

Slashing
^^^^^^^^

Like in Emmy*, not revealing nonces and double signing are punishable. If a
validator does not reveal its nonce by the end of the cycle, it does not receive
its attesting rewards. If a validator double signs, that is, it double bakes
(which means signing different blocks at the same level and same round) or it
double (pre)attests (which means voting on two different proposals at the same
level and round), a part of the frozen deposit is slashed. The slashed amount
for double baking is a fixed percentage of the frozen deposit
``PERCENTAGE_OF_FROZEN_DEPOSITS_SLASHED_PER_DOUBLE_BAKING``. For
double (pre)attestations, the formula is more complex, as it depends
on the number of attestation slots that participated in the
misbehavior; see :doc:`adaptive_slashing` for more details.
The payload producer that includes the misbehavior evidence will be rewarded a
seventh of the slashed amount, which corresponds to ``1 /
(GLOBAL_LIMIT_OF_STAKING_OVER_BAKING + 2)``.

If a delegate's deposit is smaller than the slashed amount, the deposit is
simply emptied.

The evidence for double signing at a given level can be collected by any
:ref:`accuser<def_accuser_beta>` and included as an *accusation* operation in a block
for a period of ``MAX_SLASHING_PERIOD``.

As soon as a delegate is denounced for any double signing, it is
immediately :ref:`forbidden<new_forbidden_period_beta>` from both baking
and attesting for at least 2 cycles.

The actual slashing and denunciation rewarding happen at the end of
the last cycle of the slashing period of the misbehavior.

Note that selfish baking is not an issue in Tenderbake: say we are at round
``r`` and the validator which is proposer at round ``r+1`` does not (pre)attest
at round ``r`` in the hope that the block at round ``r`` is not agreed upon and
its turn comes to propose at round ``r+1``. Under the assumption that the
correct validators have more than two thirds of the total stake, these correct
validators have sufficient power for agreement to be reached, thus the lack of
participation of a selfish baker does not have an impact.

.. _fitness_beta:

Fitness
-------

The fitness is given by the tuple ``(version, level, locked_round, - predecessor_round - 1, round)``.
The current version of the fitness is 2 (version 0 was used by Emmy, and version 1 by Emmy+ and Emmy*).
The fitness encapsulates more information than in Emmy* because Tenderbake is more complex: recall that blocks at the last level only represent :ref:`candidate blocks<finality_beta>`.
In Emmy*, only the level mattered.
But in Tenderbake, we need to, for instance, allow for new blocks at the same level to be accepted by nodes.
Therefore the fitness also includes the block's round (as the fifth component).
Furthermore, we also allow to change the predecessor block when it has a :ref:`smaller round<finality_beta>`.
Therefore the fitness also includes the opposite of predecessor block's round as the forth component (the predecessor is taken for technical reasons).
Finally, to (partially) enforce :ref:`the rule on
re-proposals<quorum_beta>`, the fitness also includes, as the third
component, the round at which a preattestation quorum was observed by
the baker, if any (this component can therefore be empty). By the way,
preattestations are present in a block if and only if the locked round
component is non-empty and if so, the locked round has to match the
round of the included preattestations.

Next, we provide two examples of fitness values:
``02::00001000::::ffffffff::00000000`` and
``02::00001000::00000000::fffffffe::00000001`` (in the hexadecimal
format that one may observe in the node's logs). These two values have
the following components:

- the 1st component, ``02``, is the fitness version;
- the 2nd component, ``00001000``, is the block's level (level 4096);
- the 3rd component is the block's locked round: empty in the first case, 0 in the second;
- the 4th component is the round of the predecessor block, here 0 in the first case and 1 in the second case;
- the 5th component is the block's round: 0 in the first case, 1 in the second case.

We recall (see :ref:`shell_header`) that the fitness is, from the
shell's perspective, a sequence of sequences of unsigned bytes and
comparison is done first by the length of the sequence and then
lexicographically (both for the outer sequence, and for each of the
inner sequences). So the first fitness is smaller than the second one,
because of the third component, the empty bitstring being smaller than
any other bitstring.

.. _cs_constants_beta:

Consensus related protocol parameters
-------------------------------------

.. list-table::
   :widths: 55 25
   :header-rows: 1

   * - Parameter name
     - Parameter value
   * - ``CONSENSUS_COMMITTEE_SIZE``
     - 7000
   * - ``CONSENSUS_THRESHOLD``
     - ``ceil(2 * CONSENSUS_COMMITTEE_SIZE / 3)`` = 4667
   * - ``MINIMAL_BLOCK_DELAY``
     - 8s
   * - ``BLOCKS_PER_CYCLE``
     - 30720
   * - ``DELAY_INCREMENT_PER_ROUND``
     - 4s
   * - ``MINIMAL_PARTICIPATION_RATIO``
     - 2/3
   * - ``MAX_SLASHING_PERIOD``
     - 2 cycles
   * - ``PERCENTAGE_OF_FROZEN_DEPOSITS_SLASHED_PER_DOUBLE_BAKING``
     - 5%
   * - ``BAKING_REWARD_FIXED_PORTION``
     - 2.67 tez
   * - ``BAKING_REWARD_BONUS_PER_SLOT``
     - ``bonus / (CONSENSUS_COMMITTEE_SIZE / 3)`` = 0.001143 tez
   * - ``ATTESTING_REWARD_PER_SLOT``
     - ``attesting_reward / CONSENSUS_COMMITTEE_SIZE`` = 0.000761 tez
   * - ``GLOBAL_LIMIT_OF_STAKING_OVER_BAKING``
     - 5

The above list of protocol parameters is a subset of the :ref:`protocol constants <protocol_constants_beta>`.

Further External Resources
--------------------------

* Tenderbake `report <https://arxiv.org/abs/2001.11965>`_
* Tenderbake `blog post <https://research-development.nomadic-labs.com/a-look-ahead-to-tenderbake.html>`_.
* Tenderbake `tzip <https://gitlab.com/tezos/tzip/-/blob/081c7691c24722ff15d2d0dfca9457f6f4d76fa2/drafts/current/draft_tenderbake.md>`_.
