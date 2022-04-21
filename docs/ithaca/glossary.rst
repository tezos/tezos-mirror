Glossary
========

This glossary is divided in two sections, the first one concerns Tezos, and
the second one concerns the `economic protocol`_. The definitions in the latter
section may be different for other protocol versions.

Tezos
-----

.. include:: ../shell/glossary.rst.h

Protocol
--------

_`Accuser`
    When a delegate_ attempts to inject several incompatible blocks (or when it tries
    to abuse the network in another similar way), another delegate_ can make an
    accusation: show evidence of attempted abuse. The delegate_ making the accusation
    is the accuser.

    The accuser is awarded some funds from the security deposit of the accused.

    When using :ref:`Octez <octez>`, accusations are handled by the
    accuser binary.

_`Account`
    An account is a unique identifier within the protocol. There are different
    kinds of accounts (see `originated account`_ and `implicit account`_).

    In the context_, each account is associated with a balance (an amount of
    tez available).

_`Baker`
    When a delegate_ creates a new block_, it is the baker of this block_.
    Baking_ rights are distributed to different accounts based on their
    available balance. Only a delegate_ with baking_ rights
    is allowed to bake.
    The baker selects transactions from the mempool_ to be included in the block_ it bakes.

    When using :ref:`Octez <octez>`, baking_ and other consensus actions are handled by the baker
    binary.

_`Baking`/_`endorsing rights`
    A delegate_ is allowed to bake/endorse a block_ if it holds the
    baking/endorsing right for that block_. At the start of a cycle_,
    baking and endorsing rights are computed for all the block_ heights in the
    cycle_, based on the proportion of the stake owned by each account.

    For each block_ height and block round_, there is exactly one account that is allowed to bake.

    When a block_ is created and propagated on the network, delegates that have
    `endorsing rights`_ for the matching block_ height can emit an endorsement
    operation_.
    Endorsement operations_ are included in the next block_.

_`Burn`
    To ensure responsible use of the storage space on the public blockchain,
    there are some costs charged to users for consuming storage. These
    costs are burnt (i.e., the amount of tez is destroyed). For example,
    a per-byte storage cost is burnt for increasing the storage space of a
    smart contract; a fixed amount is burnt for allocating a new contract
    (which consumes space by storing its address on the blockchain).

    See also `fee`_.

_`Constants`
    Protocols are parameterized by several parameters called protocol constants, which may vary from one protocol to another or from one network to another.

_`Contract`
    See account_.

_`Cycle`
    A cycle is a set of consecutive blocks. E.g., cycle 12 started at block_
    height 49152 and ended at block_ height 53248.

    Cycles are used as a unit of “time” in the block_ chain. For example, the
    different phases in the amendment voting procedures are defined based on
    cycles.

_`Delegate`
    An `implicit account`_ to which an account_ has delegated their
    rights to participate in consensus (aka baking_ rights) and in
    governance.
    The delegates' rights are calculated based on its own tokens plus the sum of tokens
    delegated to it.

_`Delegation`
    An operation_ in which an account_ balance is lent to a
    delegate_. This increases the delegate_'s stake and consequently
    its baking_ rights. The delegate_ does not control the funds from
    the account_.

_`Double signing`
    When a baker_ signs two different blocks at the same height and same round,
    it is called double baking. Double baking is detrimental to the network and might be
    indicative of an attempt to double spend.
    The same goes for signing two different endorsements at the same height and the same round.
    As such, double signing is punished by the
    network: an accuser_ can provide proof of the double signing to be awarded
    part of the double signer's deposit.

_`Fee`
    To ensure responsible use of computation resources of other nodes, and also to encourage active participation in the consensus protocol, there are some
    fees that users pay to bakers for including their operations in blocks.
    For example, fees are paid to a baker for operations such as a transaction_ or a revelation of a public key.

    See also `burn`_.

_`Gas`
    A measure of the number of elementary operations_ performed during
    the execution of a `smart contract`_. Gas is used to measure how
    much computing power is used to execute a `smart contract`_.

_`Implicit account`
    An account_ that is linked to a public key. Contrary to a `smart
    contract`_, an `Implicit account`_ cannot include a script and it
    cannot reject incoming transactions.

    If registered, an `implicit account`_ can act as a delegate_.

    The address of an `implicit account`_ always starts with the
    letters `tz` followed by `1`, `2` or `3` (depending on the
    signature scheme) and finally the hash of the public key.

.. _glossary_michelson:
.. _glossary_michelson_ithaca:

Michelson
    The built-in language used by a `smart contract`_.

_`Operations`
    The main operations in the protocol are transactions (to transfer funds
    or to execute smart contracts), accusations, activations, delegations,
    endorsements and originations.

_`Originated account`
    See `smart contract`_.

_`Origination`
    An operation_ to create a `smart contract`_.

_`Round`
    An attempt to reach consensus on a block at a given height.
    A round is represented by an index, starting with 0.
    Each round corresponds to a time span.
    A baker_ with baking_ rights at a given round is only allowed to bake during
    the round's corresponding time span. Baking_ outside of one's designated
    round results in an invalid block_.

.. _glossary_roll:

_`Roll`
    An amount of tez (e.g., 6000ꜩ) serving as a minimal amount for a
    delegate to have baking_ and voting rights in a cycle_. A roll
    also serves as a unit to determine delegates' voting rights in a
    cycle_. However, rolls are not used as a unit for baking_ rights,
    these are based on the actual, non-approximated stake.

_`Smart contract`
    Account_ which is associated to a :ref:`Michelson <glossary_michelson_ithaca>` script. They are
    created with an explicit origination_ operation and are therefore
    sometimes called originated accounts. The address of a smart
    contract always starts with the letters ``KT1``.

_`Transaction`
    An operation_ to transfer tez between two accounts, or to run the code of a
    `smart contract`_.

_`Voting period`
    Any of the ``proposal``, ``exploration``, ``cooldown``,
    ``promotion`` or ``adoption`` stages in the voting procedure when
    amending the `economic protocol`_.

_`Voting listings`
    The list calculated at the beginning of each `voting period`_ that contains
    the staking balance of each delegate_ that owns more
    than one roll_ at that moment. For each delegate_, the voting listings
    reflects the weight of the vote emitted by the delegate_ when amending the
    `economic protocol`_.
