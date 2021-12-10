The consensus algorithm
=======================

This document provides a description of Emmy*, the Tezos
:doc:`proof-of-stake<proof_of_stake>` consensus algorithm, as implemented in the
protocol under development.

History
-------

Before Emmy*, there was Emmy+
(introduced in this `blog post <https://blog.nomadic-labs.com/emmy-an-improved-consensus-algorithm.html>`_),
and before Emmy+, there was Emmy, a Nakamoto-style consensus first described in
2014, in the `Tezos whitepaper
<https://whitepaper.io/document/376/tezos-whitepaper>`_:

  our proof-of-stake mechanism is a mix of several ideas, including
  Slasher, chain-of-activity, and proof-of-burn.

The specificity of Emmy with respect to other proof-of-stake consensus
algorithms, including some protocols introduced later such as `Ouroboros
<https://eprint.iacr.org/2016/889.pdf>`_ and `Snow White
<https://eprint.iacr.org/2016/919>`_, is the combined use of priorities and
endorsements in the so called minimal delay function. Thanks to these concepts,
Emmy offers a better protection against selfish baking and a shorter time to
finality. The time to finality can be measured in terms of the number of
confirmations a user must have seen for a block to be considered as final. We
recall that, being a Nakamoto-style consensus, Emmy provides *probabilistic*
finality.


Emmy*
-----

Emmy* improves Emmy+ in that it brings smaller block times and faster times to
finality.


.. _terminology:
.. _terminology_011:

Terminology
~~~~~~~~~~~

A *block* in the blockchain consists of a header and a list of operations. The
header has a shell part (common to all protocols) and a
protocol-specific part. In Emmy*, :ref:`the protocol-specific part of the
header<emmyp_fitness_and_header_011>` contains, most notably, a
priority (a natural number). The consensus operations in a block are called *endorsements*.
These operations can be seen as votes for a given block.
The endorsements included a block at level ``l`` are votes for the block at the previous
level ``l-1``. Each block is signed.

Before being endorsed, blocks are baked. *Baking* is the action of producing and
signing a block. Corresponding to these two actions of baking and endorsing, at
each level, two lists of slots are being created: a (conceptually) infinite list
of baking slots and a list of ``ENDORSERS_PER_BLOCK`` endorsing slots (the value of ``ENDORSERS_PER_BLOCK`` is one of the :ref:`parameters of the consensus protocol <cs_constants_011>`). The index
of a baking slot is called a *priority*. Each slot is associated to a
participant. A participant can appear several times in both lists. The selection
of participants is at :ref:`random<emmyp_slot_selection_011>`, independently for
each slot, and is stake based.

An endorsement for a block at level :math:`\ell` is *valid* if it is signed by
a participant that has an endorsing slot at level :math:`\ell`. The *endorsing
power* of an endorsement is the number of slots the endorser owns at level :math:`\ell`. The endorsing
power of a block is the sum of the endorsing powers of the endorsements it
contains.


Minimal block delay function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At the heart of Emmy*, there is the minimal block delay function. This function
serves to compute the minimal time between blocks depending on the current
block's priority `p`, and its endorsing power `e`. Namely, Emmy* defines the
minimal block delay function as follows:

.. _delaystar:
.. _delaystar_011:

.. math::
   delay^*(p, e) = \begin{cases}
   md & \text{ if } p = 0 \wedge w \geq \frac{3}{5} te\\
   delay^+(p, e) & \text{ otherwise}
   \end{cases}

where

- :math:`md` stands for the minimal block delay,
- :math:`te` stands for the "total endorsing power", and
- :math:`delay^+(p, e)` is the minimal block delay function in Emmy+, namely:

.. math::
   delay^+(p, e) = bd + dp \cdot p + de \cdot max(0, ie - e)

where

- :math:`bd` stands for the minimal time between the blocks,
- :math:`dp` stands for "delay per priority",
- :math:`de` stands for "delay per (missed) endorsement", and
- :math:`ie` stands for the "initial endorsing power".

The delay function serves to determine if a produced block can
be considered as valid.

Block validity condition
~~~~~~~~~~~~~~~~~~~~~~~~

A block with timestamp :math:`t'`, priority :math:`p`, and
endorsing power :math:`e` is *valid* at level :math:`\ell` if:

- the endorsements in the block are valid for level :math:`\ell-1`,
- it is signed by the baker that has baking slot :math:`p`, and
- :math:`t' \geq t + delay^+(p,e)`, where :math:`t` is the timestamp of the
  previous block.

We note that, by the definition of the delay function, the higher the priority
and the smaller the endorsing power, the longer it takes before the block is
considered as valid. However, if the block has priority 0 and contains endorsements with endorsing
power at least :math:`ie`, then there is no time penalty.

Emmy* abstractly
~~~~~~~~~~~~~~~~

We refer to someone trying to reach consensus by the generic notion of
participant. Emmy* can be described in an abstract manner as
follows:

- A participant continuously observes blocks and endorsements.
- A participant always adopts the :ref:`fittest<emmyp_fitness_and_header_011>`, that
  is, the longest (valid) chain it observes.
- A participant that has at least an endorsement slot at level :math:`\ell`,
  emits an endorsement for the first block it observes at level
  :math:`\ell`.
- A participant produces a block as soon as it is allowed to, that is, as soon
  as it can produce a valid block (see the validity condition above).

Emmy* concretely
~~~~~~~~~~~~~~~~

In Tezos, a participant is a :ref:`delegate<delegation>` that has at least one
:ref:`roll<roll_pos_011>`, and is :ref:`active<active_delegate_011>`. For simplicity we
just refer to participants as delegates (and omit the "active" and "with rolls"
attributes).  A delegate plays two roles:

- that of a **baker**, that is, it creates blocks, or
- that of an **endorser**, that is, it contributes in agreeing on
  a block by **endorsing** that block.


.. _emmyp_slot_selection:
.. _emmyp_slot_selection_011:

To these roles correspond the two types of actions mentioned above, baking and
endorsing. As mentioned above, the baking and endorsing rights of a delegate are
given by its baking, respectively endorsing slots, whose selection is described
:ref:`here<rights_011>`. The mechanism behind baking slots is meant to ensure that
if the delegate whose turn is to bake is for some reason unable to bake, the
next delegate in the list can step up and bake the block.

.. _emmyp_fitness_and_header:
.. _emmyp_fitness_and_header_011:

There are two more notions which are defined abstractly at the level of the
shell and concretized in Emmy*, the :ref:`fitness<Score>`, and the
protocol-specific header:

- the fitness of a block is 1 plus the fitness of the previous block;
- the protocol-specific header of a block has the following fields:

  - ``signature``: a digital signature of the shell and protocol
    headers (excluding the signature itself).
  - ``priority``: the position in the priority list of delegates
    at which the block was baked.
  - ``seed_nonce_hash``: a commitment to :ref:`a random number<random_seed_011>`, used to
    generate entropy on the chain. Present in only one out of
    ``BLOCKS_PER_COMMITMENT`` (see :ref:`Constants<ps_constants_011>`).
  - ``proof_of_work_nonce``: a nonce used to pass a low-difficulty
    proof-of-work for the block, as a spam prevention measure.
  - ``liquidity_baking_escape_vote``: :ref:`a flag<esc_hatch_011>` that requests ending the subsidy.


The consensus algorithm is implemented in Tezos in five components: the shell,
the economic protocol, and the three daemons: the baker, the endorser, and the
accuser.

There are mainly two rules that the shell uses when receiving a new valid block:

- The shell changes the head of the chain to this new block only if it has a
  higher fitness than the current head.
- The shell does not accept a branch whose fork point is in a cycle more than
  ``PRESERVED_CYCLES`` in the past. More precisely, if ``n`` is the current
  cycle, the last allowed fork point is the first level of cycle
  ``n-PRESERVED_CYCLES``.

The parameter ``PRESERVED_CYCLES`` therefore plays a central role in Tezos: any
block before the last allowed fork level is immutable.

Finally, the economic protocol provides the rules for when block and
endorsements are valid, as explained above, and defines the economic incentives
of delegates. Finally, the three daemons are responsible for injecting blocks,
endorsements, and respectively accusations (see below) on behalf of delegates.


Economic Incentives
~~~~~~~~~~~~~~~~~~~

In Emmy*, participation in consensus is rewarded and bad behavior is punished.

Rewards
^^^^^^^

To incentivize participation in the consensus algorithm, delegates are rewarded
for baking and endorsing.  The reward for baking a block with priority :math:`p`
and endorsing power :math:`e` is given by the formula
:math:`baking\_reward(p,e)`.  The rewards for endorsing a block with priority
:math:`p` and having the corresponding endorsement included in the block is
given by the formula :math:`endorsing\_reward(p,e)`, where :math:`e` is the
endorsement's endorsing power.  These reward formulas are as follows:

.. math::
   baking\_reward(p,e) = \begin{cases}
   \frac{e}{te}\cdot \frac{level\_rewards\_prio\_zero}{2} & \mbox{ if } p = 0\\
   \frac{e}{te} \cdot level\_rewards\_prio\_nonzero & \mbox{ otherwise }
   \end{cases}

.. math::
   endorsing\_reward(p,e) = \begin{cases}
   baking\_reward(0, e) & \mbox{ if } p = 0\\
   \frac{2}{3} \cdot baking\_reward(0, e) & \mbox{ otherwise }
   \end{cases}

where

- :math:`level\_rewards\_prio\_zero` and :math:`level\_rewards\_prio\_nonzero` are constants.

The motivation behind this choice of design is given in the `Carthage blog post
<https://blog.nomadic-labs.com/a-new-reward-formula-for-carthage.html>`_.

Besides the reward for baking, the baker receives all the fees paid for the
transactions included in the baked block.

Rewards and fees are not distributed immediately, instead they are frozen for a
period of ``PRESERVED_CYCLES``.

Slashing
^^^^^^^^

If a delegate deviates from the consensus rules by baking or endorsing two
different blocks at the same level, we say that a delegate double signs. As a
counter-measure against double signing a *security deposit* is frozen from the
delegate's account. Precisely, each delegate key has an associated security
deposit account. When a delegate bakes or endorses a block the security deposit
is automatically moved to the deposit account where it is frozen for
``PRESERVED_CYCLES`` cycles, after which it is automatically moved back to the
baker's main account.

The values of the security deposits are ``BLOCK_SECURITY_DEPOSIT`` per block
created and ``ENDORSEMENT_SECURITY_DEPOSIT`` per endorsement slot.

The evidence for double signing at a given level can be collected by any
:ref:`accuser<Accuser>` and included as an *accusation* operation in a block
for a period of ``PRESERVED_CYCLES``. The inclusion of the accusation leads to
forfeiting the entirety of the security deposits and fees obtained during the
cycle when the double signing was made. Half of this amount is burned, and half
goes to the baker who included the accusation.

In the current protocol, accusations for the *same* incident can be made several
times after the fact. This means that the deposits and fees for the entire
cycle are forfeited, including any deposit made, or fees earned, after the
incident. Pragmatically, any baker who either double bakes or endorses in a
given cycle should immediately stop both baking and endorsing for the rest of
that cycle.

.. _cs_constants:
.. _cs_constants_011:

Consensus protocol parameters
-----------------------------

In this section we map the above notation to their corresponding parameter
values.
Note that these parameters are part of the larger set of :ref:`protocol constants <protocol_constants_011>`.

.. list-table:: Mapping
   :widths: 55 50 25
   :header-rows: 1

   * - Notation
     - Parameter name
     - Parameter value
   * - :math:`md`
     - ``MINIMAL_BLOCK_DELAY``
     - 30 seconds
   * - :math:`bd`
     - ``TIME_BETWEEN_BLOCKS[0]``
     - 60 seconds
   * - :math:`dp`
     - ``TIME_BETWEEN_BLOCKS[1]``
     - 40 seconds
   * - :math:`de`
     - ``DELAY_PER_MISSING_ENDORSEMENT``
     - 4 seconds
   * - :math:`ie`
     - ``INITIAL_ENDORSERS``
     - 192
   * - :math:`te`
     - ``ENDORSERS_PER_BLOCK``
     - 256
   * - :math:`\frac{level\_rewards\_prio\_zero}{te \cdot 2}`
     - ``BAKING_REWARD_PER_ENDORSEMENT[0]``
     - 0.078125 ꜩ
   * - :math:`\frac{level\_rewards\_prio\_nonzero}{te}`
     - ``BAKING_REWARD_PER_ENDORSEMENT[1]``
     - 0.011719 ꜩ
   * - :math:`endorsing\_reward(0,1)`
     - ``ENDORSEMENT_REWARD[0]``
     - 0.078125 ꜩ
   * - :math:`endorsing\_reward(p,1)` for :math:`p \geq 1`
     - ``ENDORSEMENT_REWARD[1]``
     - 0.052083 ꜩ
   * -
     - ``BLOCK_SECURITY_DEPOSIT``
     - 640 ꜩ
   * -
     - ``ENDORSEMENT_SECURITY_DEPOSIT``
     - 2.5 ꜩ

Since blocks are at least ``TIME_BETWEEN_BLOCKS[0]``, that is 30 seconds apart,
and since a cycle has ``BLOCKS_PER_CYCLE``, that is :ref:`8192
blocks<ps_constants_011>`, a cycle lasts *at least* 2 days, 20 hours, and 16
minutes, and ``PRESERVED_CYCLES`` cycles, that is 5 cycles, last *at least* 14
days, 5 hours, and 20 minutes.

Given that ``MINIMAL_BLOCK_DELAY`` is 30 seconds, :ref:`the minimal block delay
function<delaystar_011>` says that:

- if the block is baked at priority 0 and it contains at least 60% of the
  endorsements (namely, at least 153 endorsements) then the minimal delay is 30
  seconds;
- otherwise, the higher the priority and the fewer endorsements a block carries
  with respect to the 192 endorsements threshold, the longer it takes before it
  can be considered valid, where the delay of 60 seconds is incremented by 40
  seconds with each missed priority and with 4 seconds with each missed
  endorsement.


The value for ``BAKING_REWARD_PER_ENDORSEMENT[0]`` is chosen such that the
inflation from block rewards and endorsement rewards, which is given by
``ENDORSERS_PER_BLOCK`` \* (``ENDORSEMENT_REWARD[0]`` +
``BAKING_REWARD_PER_ENDORSEMENT[0]``) is 80 ꜩ which in turn preserves the 5.51%
annual inflation.

Since deposits are locked for a period of ``PRESERVED_CYCLES``, one can compute
that at any given time, about ((``BLOCK_SECURITY_DEPOSIT`` +
``ENDORSEMENT_SECURITY_DEPOSIT`` \* ``ENDORSERS_PER_BLOCK``) \*
(``PRESERVED_CYCLES`` + 1) \* ``BLOCKS_PER_CYCLE``) tokens of all staked tokens
should be held as security deposits. For instance, if the amount of staked
tokens is 720,000,000 ꜩ, then roughly 8.74% of this amount is stored in security
deposits. This percentage also gives an indication of the minimal amount of
tokens a delegate should own in order to not miss out on creating a block or an
endorsement. Please refer to :ref:`this section <over_delegation>`
of the documentation for a discussion on (over-)delegation.


Further External Resources
--------------------------

- Emmy* `TZIP <https://gitlab.com/tezos/tzip/-/blob/master/drafts/current/draft_emmy-star.md>`_
- Emmy* `analysis <https://blog.nomadic-labs.com/faster-finality-with-emmy.html>`_.
