.. _proof-of-stake:
.. _proof-of-stake_009:

Proof-of-stake in Tezos
=======================

:ref:`The consensus algorithm <emmy>` in Tezos is based on the **proof-of-stake**
mechanism. Proof-of-stake means that participants in the consensus algorithm are
chosen in function of their stake (the amount of coins a participant has).

If one does not have enough stake to participate on its own or does not want
to set up the needed infrastructure, (s)he can use **delegation**. Therefore, in
Tezos, participants in the consensus algorithm are called **delegates**.

Participants' rights are determined at the beginning of a **cycle** (a chunk of
blocks) by a follow-the-satoshi strategy [1]_ starting from a **random seed**
computed from information already found on the blockchain.

.. [1] The mechanism is described in `the Tezos whitepaper
       <https://tezos.com/static/white_paper-2dc8c02267a8fb86bd67a108199441bf.pdf>`_.

The remainder of this document contains a detailed description of
the notions which are in bold in the text above.

Further External Resources
~~~~~~~~~~~~~~~~~~~~~~~~~~

The original design of the proof-of-stake mechanism in Tezos can be
found in the `whitepaper
<https://tezos.com/static/white_paper-2dc8c02267a8fb86bd67a108199441bf.pdf>`_.

Another presentation of the Tezos' proof-of-stake mechanism can be found in the
`Tezos agora wiki entry <https://wiki.tezosagora.org/files/proofofstake.html>`_.

The remainder of this document contains a detailed description of
the notions which are in bold in the text above.

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
-----

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

Random seed
-----------

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
