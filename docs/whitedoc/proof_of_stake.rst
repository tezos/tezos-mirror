.. _proof-of-stake:

Proof-of-stake in Tezos
=======================

This document provides an in-depth description of the Tezos
proof-of-stake algorithm as implemented in the current protocol
(namely `PsBabyM1` on `mainnet`) and described in these blog posts
https://blog.nomadic-labs.com/analysis-of-emmy.html and
https://blog.nomadic-labs.com/emmy-an-improved-consensus-algorithm.html.


Brief Overview
--------------

A blockchain is a linked list of **blocks**. In Tezos, blocks to be
added to the blockchain are agreed upon through a proof-of-stake
consensus mechanism. Proof-of-stake means that participants in the
consensus algorithm are chosen in function of their stake. In Tezos, a
participant needs to have a minimum stake of 8,000 XTZ (which is
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
already found on blockchain.

To incentivize participation in the consensus algorithm, delegates are
**rewarded** for their baking and endorsing. As a counter-measure
against double-baking or double-endorsement a **security deposit** is
frozen from the delegate's account. The deposit is either released
after a number of cycles or burnt in case of proven bad behaviour.

The remainder of this document contains the detailed description of
the notions which are in bold in the text above.

Further External Resources
~~~~~~~~~~~~~~~~~~~~~~~~~~

The original design of the proof-of-stake mechanism in Tezos can be
found in the `whitepaper
<https://tezos.com/static/white_paper-2dc8c02267a8fb86bd67a108199441bf.pdf>`_.
Here are a few more resources that present Tezos' proof-of-stake
mechanism:

-  `Proof of Stake <https://learn.tqgroup.io/files/proofofstake.html#consensus>`_
-  `Liquid Proof-of-Stake <https://medium.com/tezos/liquid-proof-of-stake-aec2f7ef1da7>`_
-  `All you ever wanted to ask about Tezos — Illustrated <https://medium.com/@cogarius/all-you-ever-wanted-to-ask-about-tezos-illustrated-cf2034f000c9>`_


Blocks
------

The Tezos blockchain is a linked list of blocks. Blocks contain a
header, and a list of operations. The header itself decomposes into a
shell header (common to all protocols) and a protocol specific header.

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
   (``BLOCKS_PER_COMMITMENT`` = 32) blocks.
-  ``proof_of_work_nonce``: a nonce used to pass a low-difficulty
   proof-of-work for the block, as a spam prevention measure.

Block size
~~~~~~~~~~

Tezos does not download blocks all at once, but rather considers
headers and various types of operations separately.  Transactions are
limited by a total maximum size of 512kB (that is 5MB every 10 minutes
at most).

Consensus operations (endorsements, denunciations, reveals) are
limited in terms of number of operations (though the defensive
programming style also puts limits on the size of operations it
expects). This ensures that critical operations do not compete with
transactions for block space.

Cycles
------

Blocks in Tezos are grouped into *cycles* of
``BLOCKS_PER_CYCLE`` = 4,096 blocks. Since blocks are at least
``TIME_BETWEEN_BLOCKS`` = one minute apart, this means a cycle lasts *at
least* 2 days, 20 hours, and 16 minutes. In the following description,
the current cycle is referred to as ``n``, it is the nth cycle from the
beginning of the chain. Cycle ``(n-1)`` is the cycle that took place
before the current one, cycle ``(n-2)`` the one before, cycle ``(n+1)``
the one after, etc.

At any point, the tezos shell will not implicitly accept a branch whose
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
the proof-of-stake consensus algorithm and in the governance of Tezos.

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
cannot be selected for baking or endorsement.

A delegate becomes passive for cycle ``n`` when they fail to create
any of the blocks or endorsements in the past ``PRESERVED_CYCLES`` = 5
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

In theory, it would be possible to give each token a serial number,
and track the specific tokens assigned to specific delegates. However,
it would be too demanding of nodes to track assignment at such a
granular level. Instead we introduce the concept of rolls. A *roll*
represents a set of coins delegated to a given key. A roll holds
``TOKENS_PER_ROLL`` = 8,000 tokens. When tokens are moved, or a
delegate for a contract is changed, the rolls change delegate
according to the following algorithm.

Each delegate has a stack of roll ids plus some "change" which is always
an amount smaller than ``TOKENS_PER_ROLL``. When tokens are moved from
one delegate to the other, first, the change is used. If it is not
enough, rolls need to be "broken" which means that they move from the
delegate stack to a global, unallocated, roll stack. This is done until
the amount is covered, and some change possibly remains.

Then, the other delegate is credited. First the amount is added to the
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
that is 16 times per cycle. There is a tradeoff between memory
consumption and economic efficiency. If roll snapshots are too frequent,
they will consume a lot of memory. If they are too rare, strategic
participants could purchase many tokens in anticipation of a snapshot
and resell them right after.

Security deposits
~~~~~~~~~~~~~~~~~

The cost of a security deposit is ``BLOCK_SECURITY_DEPOSIT`` = 512 XTZ
per block created and ``ENDORSEMENT_SECURITY_DEPOSIT`` = 64 XTZ per
endorsement (explained below).

Each delegate key has an associated security deposit account.
When a delegate bakes or endorses a block the security deposit is
automatically moved to the deposit account where it is frozen for
``PRESERVED_CYCLES`` cycles, after which it is automatically moved
back to the baker's main account.

Since deposits are locked for a period of ``PRESERVED_CYCLES`` one can
compute that at any given time, about ((``BLOCK_SECURITY_DEPOSIT`` +
``ENDORSEMENT_SECURITY_DEPOSIT`` \* ``ENDORSERS_PER_BLOCK``) \*
(``PRESERVED_CYCLES`` + 1) \* ``BLOCKS_PER_CYCLE``) / ``763e6`` = 8.25% of
all tokens should be held as security deposits. It also means that a
delegate should own over 8.25% of the amount of token delegated to them
in order to not miss out on creating any block.

Baking
~~~~~~

Baking is the action of signing and publishing a block.
In Bitcoin, the right to publish a block is associated with solving a
proof-of-work puzzle. In Tezos, the right to publish a block in
cycle ``n`` is assigned to a randomly selected roll in a randomly
selected roll snapshot from cycle ``n-PRESERVED_CYCLES-2``.

We admit, for the time being, that the protocol generates a random
seed for each cycle. From this random seed, we can seed a
cryptographically secure pseudo-random number generator which is used
to draw baking rights for a cycle.

To each level is associated a priority list of delegates.
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

To each baking slot, we associate a list of ``ENDORSERS_PER_BLOCK`` =
32 *endorsers*. Endorsers are drawn similarly as bakers, by randomly
selecting 32 active rolls with replacement.

Each endorser verifies the last block that was baked, say at level
``n``, and emits an endorsement operation. The endorsement operations
are then baked in block ``n+1``. Once block ``n+1`` is baked, no other
endorsement for block ``n`` will be considered valid.

Minimal block delays
~~~~~~~~~~~~~~~~~~~~

A block is valid only if its timestamp has a minimal delay with
respect to the previous block’s timestamp. The minimal delay is given
by the following expression:
``TIME_BETWEEN_BLOCKS[0] + TIME_BETWEEN_BLOCKS[1] * p + DELAY_PER_MISSING_ENDORSEMENT * MAX (0, INITIAL_ENDORSERS - e)``
where ``TIME_BETWEEN_BLOCKS[0]`` = 60 seconds,
``TIME_BETWEEN_BLOCKS[1]`` = 40 seconds,
``DELAY_PER_MISSING_ENDORSEMENT`` = 8 seconds, ``INITIAL_ENDORSERS`` =
24, ``p`` is the block's priority at which the block was baked, and
``e`` is the number of endorsements the block contains. That is, the
higher the priority and the fewer endorsements a block carries the
longer it takes before it can be considered valid. However, if the
block contains more than ``INITIAL_ENDORSERS`` then there is no time
penalty.

It is possible that the same endorser be selected ``k`` times for the
same block, in this case ``k`` deposits are required and ``k`` rewards
gained. However a single operation needs to be sent on the network to
endorse ``k`` times the same block.

Rewards
~~~~~~~

Baking a block should give a block reward of ``BLOCK_REWARD /
(1 + p) * (0.8 + 0.2 * e / ENDORSERS_PER_BLOCK)`` plus all fees paid
by transactions inside the block, where ``BLOCK_REWARD`` = 16 XTZ,
``p`` is the priority at which the block was baked, and ``e`` is the
number of endorsements the block contains. Due to a bug in
``PsBabyM1``, the block reward is actually slightly smaller:
``BLOCK_REWARD * (8 + 2 * e / ENDORSERS_PER_BLOCK) / 10 / (1 + p)``,
where `/` is *integer* division.

Endorsers also receive a reward (at the same time as block creators
do). The reward is ``ENDORSEMENT_REWARD / (1 + p)``, where
``ENDORSEMENT_REWARD`` = 2 XTZ and ``p`` is the priority of the block
containing the endorsement. So the endorsement reward is only half if
it is contained in a block of priority 1.

Fitness
~~~~~~~

To each block we associate a measure of `fitness` which determines the
quality of the chain leading to that block. This measure is simply the
length of the chain (as in Bitcoin). More precisely, the fitness of a
block is 1 plus the fitness of the previous block. The shell changes
the head of the chain to the valid block has the highest fitness.


Inflation
~~~~~~~~~

Inflation from block rewards and endorsement reward is at most
``ENDORSERS_PER_BLOCK`` \* ``ENDORSEMENT_REWARD`` + ``BLOCK_REWARD`` =
80 XTZ. This means at most 5.51% annual inflation.

Random seed
~~~~~~~~~~~

Cycle ``n`` is associated with a random seed, a 256 bit number
generated at the end of cycle ``(n-PRESERVED_CYCLES-1)`` using
commitments made during cycle ``(n-PRESERVED_CYCLES-2)``, in one out
of every ``BLOCKS_PER_COMMITMENT`` = 32 blocks.

The commitment must be revealed by the original baker during cycle
``(n-PRESERVED_CYCLES-1)`` under penalty of forfeiting the rewards and
fees of the block that included the seed commitment (the associated
security deposit is not forfeited).

A *revelation* is an operation, and multiple revelations can thus be
included in a block. A baker receives a ``seed_nonce_revelation_tip`` =
1/8 XTZ reward for including a revelation.
Revelations are free operations which do not compete with transactions
for block space. Up to ``MAX_REVELATIONS_PER_BLOCK`` = 32 revelations
can be contained in any given block. Thus, 1 /
(``MAX_REVELATIONS_PER_BLOCK`` \* ``BLOCKS_PER_COMMITMENT``) = 1/1024 of
the blocks in the cycle are sufficient to include all revelations.

The revelations are hashed together to generate a random seed at the
very end of cycle ``(n-PRESERVED_CYCLES-1)``.
The seed of cycle ``(n-PRESERVED_CYCLES-2)`` is hashed with a constant
and then with each revelation of cycle ``(n-PRESERVED_CYCLES-1)``.
Once computed, this new seed is stored and used during cycle ``n``.

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
