.. _proof-of-stake:
.. _proof-of-stake_008:

Proof-of-stake in Tezos
=======================

This document provides an in-depth description of the Tezos
proof-of-stake algorithm as implemented in the current protocol
(namely `PsCARTHA` on `mainnet`).

Brief Overview
--------------

A blockchain is a linked list of **blocks**. In Tezos, blocks to be
added to the blockchain are agreed upon through a proof-of-stake
consensus mechanism. Proof-of-stake means that participants in the
consensus algorithm are chosen in function of their stake. In Tezos, a
participant needs to have a minimum stake of 8,000 ꜩ (which is
called a **roll**). If one does not have enough stake to participate
on its own or does not want to set up the needed infrastructure, (s)he
can use **delegation**. Therefore, in Tezos, participants in the
consensus algorithm are called **delegates**. There are two roles a
delegate can have: that of a **baker**, that is a delegate that
creates blocks, or that of an **endorser**, that is a delegate that
contributes in agreeing on a block by **endorsing** that block.

**Baking rights** and **endorsing rights** are determined at the
beginning of a **cycle** (a chunk of blocks) by a follow-the-satoshi
strategy starting from a **random seed** computed from information
already found on the blockchain.

To incentivize participation in the consensus algorithm, delegates are
**rewarded** for their baking and endorsing. As a counter-measure
against double-baking or double-endorsement a **security deposit** is
frozen from the delegate's account. The deposit is either released
after a number of cycles or burnt in case of proven bad behavior.

The remainder of this document contains a detailed description of
the notions which are in bold in the text above.

Further External Resources
~~~~~~~~~~~~~~~~~~~~~~~~~~

The original design of the proof-of-stake mechanism in Tezos can be
found in the `whitepaper
<https://tezos.com/static/white_paper-2dc8c02267a8fb86bd67a108199441bf.pdf>`_.
The following blog posts present the intuition behind the changes to the original consensus algorithm:

-  https://blog.nomadic-labs.com/analysis-of-emmy.html,
-  https://blog.nomadic-labs.com/emmy-an-improved-consensus-algorithm.html,
-  https://blog.nomadic-labs.com/a-new-reward-formula-for-carthage.html.

Here are a few more resources that present Tezos' proof-of-stake
mechanism:

-  `Proof of Stake <https://learn.tqtezos.com/files/proofofstake.html#consensus>`_
-  `Liquid Proof-of-Stake <https://medium.com/tezos/liquid-proof-of-stake-aec2f7ef1da7>`_

Please note that these external resources may contain outdated information.

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

Cycles
------

Blocks in Tezos are grouped into *cycles* of
``BLOCKS_PER_CYCLE`` = 4,096 blocks. Since blocks are at least
``TIME_BETWEEN_BLOCKS[0]`` = one minute apart, this means a cycle lasts *at
least* 2 days, 20 hours, and 16 minutes. In the following description,
the current cycle is referred to as ``n``, it is the ``n``-th cycle from the
beginning of the chain. Cycle ``(n-1)`` is the cycle that took place
before the current one, cycle ``(n-2)`` the one before, cycle ``(n+1)``
the one after, etc.

At any point, the shell will not implicitly accept a branch whose
fork point is in a cycle more than ``PRESERVED_CYCLES`` = 5 cycles in the
past (that is *at least* 14 days, 5 hours, and 20 minutes).

Delegation
----------

Tezos uses a delegated proof-of-stake model. The acronym DPOS has come to
designate a specific type of algorithm used, for instance in Bitshares.
This is *not* the model used in Tezos, though there is a concept
of delegation.

Delegates
~~~~~~~~~

Tokens are controlled through a private key called the
*manager key*. Tezos accounts let the manager specify a public
delegate key. This key may be controlled by the managers themselves, or
by another party. The responsibility of the delegate is to take part in
the proof-of-stake consensus algorithm and the governance of Tezos.

The manager can generally change the delegate at any time, though
contracts can be marked to specify an immutable delegate. Though
delegation can be changed dynamically, the change only becomes effective
after a few cycles.

There are also default accounts in Tezos, which are just the hash of the
public key. These accounts do not have an attached delegate key and do
not participate in the proof-of-stake algorithm.

Finally, delegate accounts (used for placing safety deposits) are
automatically delegated to the delegate itself.

Active and passive delegates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A delegate can be marked as either active or passive. A passive delegate
cannot be selected for baking or endorsing.

A delegate becomes passive for cycle ``n`` when they fail to create
any blocks or endorsements in the past ``PRESERVED_CYCLES``
cycles, that is, in cycles ``n-1``, ``n-2``, ..., ``n -
PRESERVED_CYCLES``.

Discussion: giving ``PRESERVED_CYCLES`` a small value means
the chain adapts more quickly to participants disappearing. It's not
unlike the "difficulty adjustment" of Bitcoin. However, a long value
would ensure that a minority fork progresses more slowly for a longer
period of time than the majority fork. ``PRESERVED_CYCLES``
gives the majority chain a "headstart".

Rolls
~~~~~

In theory, it would be possible to give each token a serial number
and track the specific tokens assigned to specific delegates. However,
it would be too demanding of nodes to track assignments at such a
granular level. Instead, we introduce the concept of rolls. A *roll*
represents a set of coins delegated to a given key. A roll holds
``TOKENS_PER_ROLL`` = 8,000 tokens. When tokens are moved, or a
delegate for a contract is changed, the rolls change delegate
according to the following algorithm.

Each delegate has a stack of roll identifiers plus some "change" which is always
an amount smaller than ``TOKENS_PER_ROLL``. When tokens are moved from
one delegate to the other, first, the change is used. If it is not
enough, rolls need to be "broken" which means that they move from the
delegate stack to a global, unallocated, roll stack. This is done until
the amount is covered, and some change possibly remains.

Then, the other delegate is credited. First, the amount is added to the
"change". If it becomes greater than ``TOKENS_PER_ROLL``, then rolls
are unstacked from the global unallocated roll stack onto the delegate
stack. If the global stack is empty, a fresh roll is created.

This preserves the property that if the delegate is changed through
several transactions, the roll assignment is preserved, even if each
operation moves less than a full roll.

The advantage of tracking tokens in this way is that a delegate creating
a malicious fork cannot easily change the specific rolls assigned to
them, even if they control the underlying tokens and shuffle them
around.

Roll snapshots
~~~~~~~~~~~~~~

Roll snapshots represent the state of rolls for a given block. Roll
snapshots are taken every ``BLOCKS_PER_ROLL_SNAPSHOT`` = 256 blocks,
which is 16 times per cycle. There is a tradeoff between memory
consumption and economic efficiency. If roll snapshots are too frequent,
they will consume a lot of memory. If they are too rare, strategic
participants could purchase many tokens in anticipation of a snapshot
and resell them right after.

Baking
~~~~~~

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
~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~

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
blocks of priority 1 or higher will be rewarded ``e * 0.833333``
ꜩ.

Security deposits
~~~~~~~~~~~~~~~~~

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
~~~~~~~~~

Inflation from block rewards and endorsement reward is at most
``ENDORSERS_PER_BLOCK`` \* (``ENDORSEMENT_REWARD[0]`` +
``BAKING_REWARD_PER_ENDORSEMENT[0]``) =
80 ꜩ. This means at most 5.51% annual inflation.

Random seed
~~~~~~~~~~~

Each cycle ``n`` is associated with a random seed.  This seed is used to
randomly select a roll snapshot from cycle ``n-2`` and to randomly
select rolls in the selected snapshot. The selected rolls determine
the baking and endorsing rights in cycle ``n+PRESERVED_CYCLES``.

The random seed for cycle ``n`` is a 256-bit number generated at the
very end of cycle ``n-1`` from nonces to which delegates commit during
cycle ``n-2``. One out of every ``BLOCKS_PER_COMMITMENT`` = 32 blocks
can contain a commitment. There are therefore at most
``BLOCKS_PER_CYCLE / BLOCKS_PER_COMMITMENT`` = 128 commitments. A
commitment is the hash of a nonce. The commitment is generated by the
baker who produces the block and is included in the block header.

The committed nonce must be revealed by the original baker during
cycle ``n-1`` under penalty of forfeiting the rewards and fees of the
block that included the commitment. The associated security deposit is
not forfeited.

A *nonce revelation* is an operation, and multiple nonce revelations
can thus be included in a block. A baker receives a
``SEED_NONCE_REVELATION_TIP`` = 1/8 ꜩ reward for including a
revelation. Revelations are free operations which do not compete with
transactions for block space. Up to ``MAX_ANON_OPS_PER_BLOCK`` = 132
revelations, wallet activations and denunciations can be contained in any
given block.

The seed for cycle ``n`` is obtained as follows: the seed of cycle
``n-1`` is hashed with a constant and then with each nonce revealed in
cycle ``n-1``.

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
