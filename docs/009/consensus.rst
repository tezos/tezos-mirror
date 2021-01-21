.. _emmy:

The consensus algorithm in Tezos
================================

This document provides a description of Emmy
:math:`\hspace{-.1cm}`:superscript:`+`, the Tezos proof-of-stake consensus
algorithm, as implemented in the current protocol (namely `PsFloren` on
`mainnet`).

History
-------

Emmy [1]_ is a Nakamoto-style consensus first described in 2014, in the `Tezos
whitepaper
<https://whitepaper.io/document/376/tezos-whitepaper>`_:

  our proof-of-stake mechanism is a mix of several ideas, including Slasher,
  chain-of-activity, and proof-of-burn.


Two years after, in 2016, the `Ouroboros paper
<https://eprint.iacr.org/2016/889.pdf>`_ appeared, 10 days prior to the `Snow
White paper <https://eprint.iacr.org/2016/919>`_, which describe a consensus
algorithm that can be seen somewhat as:

- a direct translation of Bitcoin into a proof of stake system, and
- a simplification of Emmy.

The specificity of Emmy is the combined use of priorities and endorsements in
the so called minimal delay function. Thanks to these concepts, Emmy offers a
better protection against selfish baking and "shorter time to finality
[2]_". The time to finality can be measured in terms of the number of
confirmations a user must have seen for a block to be considered as final.

Emmy :superscript:`+` came in 2019 as `an improvement over Emmy
<https://blog.nomadic-labs.com/emmy-an-improved-consensus-algorithm.html>`_.

.. [1] The name "Emmy" came later in `this post <https://blog.nomadic-labs.com/emmy-an-improved-consensus-algorithm.html>`_.

.. [2] Being a Nakamoto-style consensus, Emmy provides probabilistic finality.

Emmy :superscript:`+`
------------------------

Terminology
~~~~~~~~~~~

An **endorsement** of a block is an operation consisting of a signature on that
block.

Each block has a timestamp, a priority (a natural number), and a number of
endorsements for the block at the previous level. Each block is signed.

At each level, two lists of **slots** are built: a (conceptually) infinite list
of baking slots and a list of 32 endorsing slots. A participant can appear several
times in both lists. The selection of participants is at random, independently for each
slot. The chances for a participant to be selected is proportional with the participant's stake.

An endorsement for a block at level :math:`\ell` is **valid** if it is signed by
a participant that has an endorsement slot at level :math:`\ell`.

Minimal block delay function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At the heart of Emmy :superscript:`+`, there is the delay function. This function
serves to compute the minimal time between blocks depending on the current block's
priority `p`, and the number of endorsements `e` included in the current block.
Namely, Emmy :superscript:`+` defines the minimal block delay function as follows:

.. math::
   delay(p, e) = bd + dp \cdot p + de \cdot max(0, ie - e)

where

- :math:`bd` stands for the minimal time between the blocks,
- :math:`dp` stands for "delay per priority",
- :math:`de` stands for "delay per (missed) endorsement" , and
- :math:`ie` stands for number of "initial endorsements" (a constant representing a threshold on the number of endorsements).

The delay function serves to determine if a produced block can be considered as
valid.

Block validity condition
~~~~~~~~~~~~~~~~~~~~~~~~

A block with timestamp :math:`t'`, priority :math:`p`, and :math:`e` endorsements is **valid** at level :math:`\ell` if:

- the endorsements are valid for level :math:`\ell-1`,
- it is signed by the baker that has baking slot :math:`p`, and
- :math:`t' \geq t + delay(p,e)`, where :math:`t` is the timestamp of the
  previous block

Emmy :superscript:`+` abstractly
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We refer to someone trying to reach consensus by the generic notion of
participant. Emmy :superscript:`+` can be described in an abstract manner as
follows:

- A participant continuously observes blocks and endorsements.
- A participant always adopts the longest (valid) chain it observes.
- A participant that has :math:`e` endorsement slots at level :math:`\ell`, emits
  :math:`e` endorsements for the first block it observes at level :math:`\ell`.
- A participant produces a block as soon as it is allowed to, that is, as
  soon as it can produce a valid block (see the validity condition
  above).

Emmy :superscript:`+` concretely
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Tezos, a participant:

- is identified by a public key and signs blocks with the corresponding private key
- needs to have a minimum stake [3]_ of 8,000 ꜩ (which is called a **roll**)
- needs to be active

.. [3] Recall that Emmy :superscript:`+`, like Emmy, is :ref:`proof-of-stake <proof-of-stake>` based.

There are two roles a participant can have:

- that of a **baker**, that is a participant that creates blocks, or
- that of an **endorser**, that is a participant that contributes in agreeing on
  a block by **endorsing** that block.

Correspondingly, there are two rights, for **baking** and **endorsing**
determined as described in the entry :ref:`proof-of-stake`.

Incentives
~~~~~~~~~~

To incentivize participation in the consensus algorithm, delegates are
**rewarded** for their baking and endorsing. The current reward formulas are as
follows:

.. math::
   block\_reward(p,e) = \begin{cases}
   \frac{e}{32} \cdot 80 \cdot \frac{1}{2} & \mbox{ if } p = 0\\
   \frac{e}{32} \cdot 6 & \mbox{ otherwise }
   \end{cases}

.. math::
   endorsements\_reward(p,e) = \begin{cases}
   block\_reward(0, e) & \mbox{ if } p = 0\\
   \frac{2}{3} \cdot block\_reward(0, e) & \mbox{ otherwise }
   \end{cases}

where 80 represents the block reward. The motivation behind this choice of
design is given in the `Carthage post
<https://blog.nomadic-labs.com/a-new-reward-formula-for-carthage.html>`_.

As a counter-measure against double-baking or double-endorsement a **security
deposit** is frozen from the delegate's account. The deposit is either released
after a number of cycles or burnt in case of proven bad behavior.

Further External Resources
--------------------------

The following blog posts present the intuition behind Emmy :superscript:`+`:

-  https://blog.nomadic-labs.com/emmy-an-improved-consensus-algorithm.html
-  https://blog.nomadic-labs.com/a-new-reward-formula-for-carthage.html.

Emmy :superscript:`+` was further analyzed in:

-  https://blog.nomadic-labs.com/analysis-of-emmy.html
-  https://blog.nomadic-labs.com/on-defending-against-malicious-reorgs-in-tezos-proof-of-stake.html
-  https://blog.nomadic-labs.com/emmy-in-the-partial-synchrony-model.html
-  https://blog.nomadic-labs.com/the-case-of-mixed-forks-in-emmy.html

A more high-level presentation of Emmy :superscript:`+` can be found in the
`Tezos agora wiki entry
<https://wiki.tezosagora.org/learn/baking/proofofstake/consensus>`_.


The remainder of this document contains a detailed description of the notions
which are in bold in the text above.

Blocks
------

The Tezos blockchain is a linked list of blocks. Blocks contain a
header and a list of operations. The header itself decomposes into a
shell header (common to all protocols) and a protocol-specific header.

Shell header
~~~~~~~~~~~~

The shell header contains

-  ``level``: the height of the block, from the genesis block
-  ``proto``: number of protocol changes since genesis (mod 256)
-  ``predecessor``: the hash of the preceding block.
-  ``timestamp``: the timestamp at which the block is claimed to have
   been created.
-  ``validation_pass``: number of validation passes (also number of
   lists of lists of operations)
-  ``fitness``: a sequence of sequences of unsigned bytes, ordered by
   length and then lexicographically. It represents the claimed fitness
   of the chain ending in this block.
-  ``operations_hash``: the hash of a list of root hashes of Merkle
   trees of operations. There is one list of operations per
   validation pass.
-  ``context`` Hash of the state of the context after application of
   this block.

Protocol header
~~~~~~~~~~~~~~~

-  ``signature``: a digital signature of the shell and protocol headers
   (excluding the signature itself).
-  ``priority``: the position in the priority list of delegates at which
   the block was baked.
-  ``seed_nonce_hash``: a commitment to a random number, used to
   generate entropy on the chain. Present in only one out of
   ``BLOCKS_PER_COMMITMENT`` = 32 blocks.
-  ``proof_of_work_nonce``: a nonce used to pass a low-difficulty
   proof-of-work for the block, as a spam prevention measure.

Block size
~~~~~~~~~~

Tezos does not download blocks all at once but rather considers
headers and various types of operations separately.  Transactions are
limited by a total maximum size of 512kB (that is 5MB every 10 minutes
at most).

Consensus operations (endorsements, denunciations, reveals) are
limited in terms of number of operations (though the defensive
programming style also puts limits on the size of operations it
expects). This ensures that critical operations do not compete with
transactions for block space.

Fitness
~~~~~~~

To each block, we associate a measure of `fitness` which determines the
quality of the chain leading to that block. This measure is simply the
length of the chain (as in Bitcoin). More precisely, the fitness of a
block is 1 plus the fitness of the previous block. The shell changes
the head of the chain to the valid block that has the highest fitness.

Baking
------

Baking is the action of producing and signing a block.
In Bitcoin, the right to produce a block is associated with solving a
proof-of-work puzzle. In Tezos, the right to produce a block in
cycle ``n`` is assigned to a randomly selected roll in a randomly
selected roll snapshot from cycle ``n-PRESERVED_CYCLES-2``.

We admit, for the time being, that the protocol generates a random
seed for each cycle. From this random seed, we can seed a
cryptographically secure pseudo-random number generator which is used
to draw baking rights for a cycle.

Each level is associated with a priority list of delegates.
This list is obtained by randomly selecting an active roll for each
position in the list, and then taking the owner of the selected roll.
As the draw is independent for each list position, it is possible that
the same public key appears multiple times in this list.
The first baker in the list is the first one who can bake a block at
that level.
If a delegate is for some reason unable to bake, the next delegate in
the list can step up and bake the block.
The elements of the list that contain a certain delegate are also
called the *baking slots* of that delegate, and the indexes of these
slots are called *priorities*.

Baking a block gives a block reward (detailed below) plus
all fees paid by transactions inside the block.

Endorsements
------------

To each level, we associate a list of ``ENDORSERS_PER_BLOCK`` =
32 *endorsers*. Endorsers are drawn similarly as bakers, by randomly
selecting 32 active rolls with replacement.

Each endorser verifies the last block that was baked, say at the level
``n``, and emits an endorsement operation. The endorsement operations
are then baked in block ``n+1``. Once block ``n+1`` is baked, no other
endorsement for block ``n`` will be considered valid.

An endorser may have more than one endorsement
slot. However, the endorser injects a single endorsement operation,
which represents all of its endorsement slots. In what follows, when
we say "the number of endorsements a block contains", we do not refer
to the number of endorsement operations, but to the number of
endorsement slots covered by the contained endorsement
operations. (In the code base, the number of filled endorsement slots
is called the block's endorsing power.)

Minimal block delays
--------------------

A block is valid only if its timestamp has a minimal delay with
respect to the previous block’s timestamp. The minimal delay is given
by the following expression: ``TIME_BETWEEN_BLOCKS[0] +
TIME_BETWEEN_BLOCKS[1] * p +`` ``DELAY_PER_MISSING_ENDORSEMENT * MAX
(0, INITIAL_ENDORSERS - e)`` where ``TIME_BETWEEN_BLOCKS[0]`` = 60
seconds, ``TIME_BETWEEN_BLOCKS[1]`` = 40 seconds,
``DELAY_PER_MISSING_ENDORSEMENT`` = 8 seconds, ``INITIAL_ENDORSERS`` =
24, ``p`` is the block's priority at which the block was baked, and
``e`` is the number of endorsements the block contains. That is, the
higher the priority and the fewer endorsements a block carries the
longer it takes before it can be considered valid. However, if the
block contains more than ``INITIAL_ENDORSERS`` then there is no time
penalty.

Rewards
-------

Baking a block gives a block reward of ``e *
BAKING_REWARD_PER_ENDORSEMENT[p']`` plus all fees paid by the
transactions contained in the block, where
``BAKING_REWARD_PER_ENDORSEMENT`` = ``[1.250ꜩ, 0.1875ꜩ]``,
``e`` is the number of endorsements the block contains, ``p`` is the
priority at which the block was baked, and ``p'`` is 0 if ``p`` is
0 and is 1 if ``p`` is bigger than 0.  That is, a delegate
producing a block of priority 0 will be rewarded ``e * 1.25``
ꜩ. If a delegate produces a block at priority 1 or higher, then
the reward is ``e * 0.1875`` ꜩ.

Endorsers also receive a reward (at the same time as block creators
do). The reward is ``ENDORSEMENT_REWARD[p']``, where
``ENDORSEMENT_REWARD`` = ``[1.250ꜩ, 0.833333ꜩ]``, where ``p'``
is as above.  That is, a delegate endorsing a block of priority 0
will be rewarded ``e * 1.25`` ꜩ, with ``e`` the number of endorsement
slots attributed to the delegate for this level. Moreover, endorsing
blocks of priority 1 or higher will be rewarded ``e * 0.8333333``
ꜩ.

Security deposits
-----------------

The cost of a security deposit is ``BLOCK_SECURITY_DEPOSIT`` = 512 ꜩ
per block created and ``ENDORSEMENT_SECURITY_DEPOSIT`` = 64 ꜩ per
endorsement slot.

Each delegate key has an associated security deposit account.
When a delegate bakes or endorses a block the security deposit is
automatically moved to the deposit account where it is frozen for
``PRESERVED_CYCLES`` cycles, after which it is automatically moved
back to the baker's main account.

Since deposits are locked for a period of ``PRESERVED_CYCLES`` one can
compute that at any given time, about ((``BLOCK_SECURITY_DEPOSIT`` +
``ENDORSEMENT_SECURITY_DEPOSIT`` \* ``ENDORSERS_PER_BLOCK``) \*
(``PRESERVED_CYCLES`` + 1) \* ``BLOCKS_PER_CYCLE``) tokens of all
staked tokens should be held as security deposits. For instance, if
the amount of staked tokens is 720,000,000 ꜩ, then roughly 8.74% of
this amount is stored in security deposits. This percentage also gives
an indication of the minimal amount of tokens a delegate should own in
order to not miss out on creating a block or an endorsement.  Please
refer to `this section
<https://tezos.gitlab.io/introduction/howtorun.html#deposits-and-over-delegation>`_
of the documentation for a discussion on (over-)delegation.

Inflation
---------

Inflation from block rewards and endorsement reward is at most
``ENDORSERS_PER_BLOCK`` \* (``ENDORSEMENT_REWARD[0]`` +
``BAKING_REWARD_PER_ENDORSEMENT[0]``) =
80 ꜩ. This means at most 5.51% annual inflation.

Accusations
-----------

If two endorsements are made for the same slot or two blocks at the
same height by a delegate, the evidence can be collected by an accuser
and included in a block for a period of ``PRESERVED_CYCLES``,
including the current cycle.

This accusation forfeits the entirety of the safety deposit and future
reward up to that point in the cycle. Half is burned, half goes to the
accuser in the form of a block reward.

In the current protocol, accusations for the *same* incident can be
made several times after the fact. This means that the deposits and
rewards for the entire cycle are forfeited, including any deposit
made, or reward earned, after the incident.

Pragmatically, any baker who either double bakes or endorses in a
given cycle should immediately stop both baking and endorsing for the
rest of that cycle.
