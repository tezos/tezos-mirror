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
`PBFT <https://www.scs.stanford.edu/nyu/03sp/sched/bfs.pdf>`_ or
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

.. _tb_validator_tallinn:

Tenderbake is executed for each new block level by a "committee" whose members
are called *validators*, which are delegates selected at random based on their
stake, in much the same way as endorsers were selected in Emmy*. We let
``CONSENSUS_COMMITTEE_SIZE`` be the number of validator :ref:`slots<rights_tallinn>` per level.
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

.. _candidate_block_tallinn:

* a validator designated for that round injects a *candidate block* (representing a proposal) and consensus operations (representing votes) into the node to which it is attached, which then
* diffuses those blocks and consensus operations to other nodes of the network, and thus
* communicates them to the validators attached to those nodes, to carry out voting on which block to accept.

.. _quorum_tallinn:

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


.. _finality_tallinn:

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

.. _time_between_blocks_tallinn:

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


.. _active_stake_tallinn:

Validator selection
-------------------

Validator selection, that is, baking and attesting rights, are based
on the :doc:`baking power<baking_power>` of a delegate. The baking
power is a function of all tez owned by the delegate and its
delegators, with :doc:`staked<staking>` tez weighted more than
non-staked tez, and there are additional considerations such as
overstaking and overdelegation; see the :ref:`baking power
formula<baking_power_overview_tallinn>`.

The baking rights are determined
:ref:`CONSENSUS_RIGHTS_DELAY<cs_constants_tallinn>` in advance, which is
currently ``2`` :ref:`cycles<def_cycle_tallinn>`. More
precisely, at the end of cycle ``n`` and beginning of cycle ``n+1``,
the baking rights for cycle ``n+1+CONSENSUS_RIGHTS_DELAY=n+3`` are
:doc:`randomly generated<randomness_generation>` based on the current
:doc:`baking power<baking_power>` of each delegate that meets the
:ref:`minimal power and own staked
requirements<minimal_baking_power_tallinn>`.


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
  - the selection of the consensus committee cycle ``c + CONSENSUS_RIGHTS_DELAY``, based on the current distribution of baking power.


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

Fees are allocated to the payload producer's available balance immediately, that is, they are
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
Attesting rewards may be received even if not all of the validator's attestations are included in the cycle's blocks and is proportional to the validator's baking power (in other words, to its *expected* number of validator slots, and not its actual number of slots).
However, two conditions must be met:

- the validator has revealed its nonces, and
- the validator has been present during the cycle.

Not giving rewards in case of missing revelations is not new as it is :ref:`adapted<random_seed_tallinn>`
from Emmy*.
The second condition is new. We say that a delegate is *present* during a cycle
if the attesting power (that is, the number of validator slots at the
corresponding level) of all the delegate's attestations included during the
cycle represents at least ``MINIMAL_PARTICIPATION_RATIO`` of the delegate's expected number of
validator slots for the current cycle (which is ``BLOCKS_PER_CYCLE *
CONSENSUS_COMMITTEE_SIZE * active_stake / total_active_stake``).

The concrete values for rewards depend on the issuance which is dynamically adjusted by :ref:`Adaptive Issuance<adaptive_issuance_tallinn>`.
For each block it issues an amount ``total_rewards`` of rewarded tez, that varies with
the total amount of tez at stake on the chain.
To obtain some concrete values, we will use as an example the issuance before Adaptive Issuance,
which was ``80`` tez per minute. With ``MINIMAL_BLOCK_DELAY = 6s``, this corresponds to a ``total_rewards`` per level of 8 tez.
We define:

- ``baking_reward_fixed_portion := baking_reward_ratio * total_rewards``
- ``bonus := (1 - baking_reward_ratio) * bonus_ratio * total_rewards`` is the max bonus
- ``attesting_reward := (1 - baking_reward_ratio) * (1 - bonus_ratio) * total_rewards``

where:

- ``baking_reward_ratio`` to ``1 / 4``,
- ``bonus_ratio`` to ``1 / 3``.

Thus, we obtain ``baking_reward_fixed_portion = 2`` tez,
(maximum) ``bonus = 2`` tez, and ``attesting_reward = 4`` tez.
The bonus per additional attestation slot is in turn ``bonus /
(CONSENSUS_COMMITTEE_SIZE / 3)`` (because there are at most
``CONSENSUS_COMMITTEE_SIZE / 3`` validator slots corresponding to the
additional attestations included in a block). The rewards per
attestation slot are ``attesting_reward / CONSENSUS_COMMITTEE_SIZE``.
Assuming ``CONSENSUS_COMMITTEE_SIZE = 7000``, we obtain a bonus per slot of
``2 / (7000 / 3) = 0.0008571`` tez and an attesting
rewards per slot of ``4 / 7000 = 0.000571`` tez.

Let's take an example. Say a block has round 1, is proposed by
delegate B, and contains the payload from round 0 produced by delegate
A. Also, B includes attestations with attesting power ``5251``. Then A receives
the fees and 2 tez (the ``baking_reward_fixed_portion``) as a reward for
producing the block's payload. Concerning the bonus, given that
``CONSENSUS_COMMITTEE_SIZE = 7000``, the minimum required validator slots is ``4667``, and there are ``2333 = 7000 - 4667`` additional validator slots.
Therefore B receives the bonus ``(5251 - 4667) * 0.0008571 = 0.5005464`` tez. (Note
that B only included attestations corresponding to ``584 = 5251 - 4667`` additional validator slots, about a quarter of the
maximum ``2333`` extra attestations it could have theoretically included.) Finally, consider some
delegate C, whose baking power at some cycle is 1% of the total stake. Note that
his expected number of validator slots for that cycle is
``1/100 * BLOCKS_PER_CYCLE * CONSENSUS_COMMITTEE_SIZE = 1/100 * 14400 * 7000 = 1,008,000``
slots. Assume also that the attesting power of C's attestations
included during that cycle has been ``673,456`` slots. Given that this number is
bigger than the minimum required (``1,008,000 * 2 / 3``), it receives an attesting
reward of ``1,008,000 * 0.000571 = 575.568`` tez for that cycle.

.. _slashing_tallinn:

Slashing
^^^^^^^^

Like in Emmy*, not revealing nonces and double signing are punishable. If a
validator does not reveal its nonces by the end of the cycle, it does not receive
its attesting rewards. If a validator double signs, that is, it double bakes
(which means signing different blocks at the same level and same round) or it
double (pre)attests (which means voting on two different proposals at the same
level and round), a part of the frozen deposit is slashed. The slashed amount
for double baking is a fixed percentage of the frozen deposit
``PERCENTAGE_OF_FROZEN_DEPOSITS_SLASHED_PER_DOUBLE_BAKING``. For
double (pre)attestations, the formula is more complex, as it depends
on the number of attestation slots that participated in the
misbehavior; see :doc:`adaptive_slashing` for more details.
The payload producer that includes the misbehavior evidence is
rewarded ``1 / (GLOBAL_LIMIT_OF_STAKING_OVER_BAKING + 2)`` of the
slashed amount; the rest of the slashed amount is burned.

If a delegate's deposit is smaller than the slashed amount, the deposit is
simply emptied.

The evidence for double signing at a given level can be collected by
any :ref:`accuser<def_accuser_tallinn>` and included as a *denunciation*
operation in a block in the same cycle as the double signing or in the
``DENUNCIATION_PERIOD`` next cycles.
The actual slashing and denunciation rewarding happen at the end of
cycle ``n + SLASHING_DELAY`` for a misbehavior that happened in cycle
``n``.

As soon as a delegate is denounced for any double signing, it is immediately :ref:`forbidden<new_forbidden_period_tallinn>` from both baking and attesting for at least 2 cycles.

The baker stays forbidden from the denunciation time to the slashing itself, which is the remainder of the current cycle and least one full cycle.
Then, the baker remains forbidden until its current stake matches the computed rights of the cycle.
Because it has been slashed, its frozen stake is lower during the first cycle after slashing, so it remains forbidden for at least one more cycle.
In this way, staking more tez can reduce the number of cycles in which it can't participate in consensus.

Also note that while forbidden, a baker might become deactivated from inactivity, and might need to reactivate manually.

Note that selfish baking is not an issue in Tenderbake: say we are at round
``r`` and the validator which is proposer at round ``r+1`` does not (pre)attest
at round ``r`` in the hope that the block at round ``r`` is not agreed upon and
its turn comes to propose at round ``r+1``. Under the assumption that the
correct validators have more than two thirds of the total stake, these correct
validators have sufficient power for agreement to be reached, thus the lack of
participation of a selfish baker does not have an impact.

.. _fitness_section_tallinn:

Fitness
-------

The fitness is given by the tuple ``(version, level, locked_round, - predecessor_round - 1, round)``.
The current version of the fitness is 2 (version 0 was used by Emmy, and version 1 by Emmy+ and Emmy*).
The fitness encapsulates more information than in Emmy* because Tenderbake is more complex: recall that blocks at the last level only represent :ref:`candidate blocks<finality_tallinn>`.
In Emmy*, only the level mattered.
But in Tenderbake, we need to, for instance, allow for new blocks at the same level to be accepted by nodes.
Therefore the fitness also includes the block's round (as the fifth component).
Furthermore, we also allow to change the predecessor block when it has a :ref:`smaller round<finality_tallinn>`.
Therefore the fitness also includes the opposite of predecessor block's round as the forth component (the predecessor is taken for technical reasons).
Finally, to (partially) enforce :ref:`the rule on
re-proposals<quorum_tallinn>`, the fitness also includes, as the third
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

.. _cs_constants_tallinn:

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
   * - ``DENUNCIATION_PERIOD``
     - 1 cycle
   * - ``MINIMAL_BLOCK_DELAY``
     - 6s
   * - ``BLOCKS_PER_CYCLE``
     - 14400
   * - ``DELAY_INCREMENT_PER_ROUND``
     - 3s
   * - ``CONSENSUS_RIGHTS_DELAY``
     - 2 cycles
   * - ``GLOBAL_LIMIT_OF_STAKING_OVER_BAKING``
     - 9
   * - ``LIMIT_OF_DELEGATION_OVER_BAKING``
     - 9
   * - ``MINIMAL_STAKE``
     - 6,000 ꜩ
   * - ``MINIMAL_FROZEN_STAKE``
     - 600 ꜩ
   * - ``MINIMAL_PARTICIPATION_RATIO``
     - 2/3
   * - ``PERCENTAGE_OF_FROZEN_DEPOSITS_SLASHED_PER_DOUBLE_BAKING``
     - 5%
   * - ``SLASHING_DELAY``
     - 1 cycle
   * - ``CONSENSUS_KEY_ACTIVATION_DELAY`` [#derived_cs]_
     - 2 cycles
   * - ``ISSUANCE_MODIFICATION_DELAY`` [#derived_cs]_
     - 2 cycles
   * - ``UNSTAKE_FINALIZATION_DELAY`` [#derived_cs+sd]_
     - 3 cycles

The above list of protocol parameters is a subset of the :ref:`protocol constants <protocol_constants_tallinn>`.

.. [#derived_cs] These :ref:`derived constants
                 <protocol_constants_tallinn>` are automatically set to
                 the same value as ``CONSENSUS_RIGHTS_DELAY``.

.. [#derived_cs+sd] This :ref:`derived constant
                    <protocol_constants_tallinn>` is automatically set
                    to ``CONSENSUS_RIGHTS_DELAY + SLASHING_DELAY``.


Further External Resources
--------------------------

* Tenderbake `report <https://arxiv.org/abs/2001.11965>`_
* Tenderbake `blog post <https://research-development.nomadic-labs.com/a-look-ahead-to-tenderbake.html>`_.
* Tenderbake `tzip <https://gitlab.com/tezos/tzip/-/blob/081c7691c24722ff15d2d0dfca9457f6f4d76fa2/drafts/current/draft_tenderbake.md>`_.
