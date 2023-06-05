Glossary
========

This glossary is divided in two sections, the first one concerns Tezos, and
the second one concerns the `economic protocol`_. The definitions in the latter
section may be different for other protocol versions.

Tezos
-----

_`Block`
    The Tezos blockchain is a linked list of blocks (or actually, a tree when several competing branches exist).
    Blocks conceptually contain a header and a list of operation_\ s,
    which are specific to the `economic protocol`_.

    The header itself decomposes into a :ref:`shell header<shell_header>` (common to all Tezos economic protocols), and a protocol-specific header.
    The shell header contains protocol-agnostic data such as the predecessor's block hash and the block's timestamp.

.. _def_context_mumbai:

_`Context`
    The state of the blockchain. The context is defined by the
    `economic protocol`_ and typically includes information such as
    “this account_ is credited with this many tez” and “this is the
    code for that `smart contract`_.”

    The context is modified by operation_\ s. For example, an
    operation_ can transfer tez from one account_ to another, which modifies the
    part of the context that tracks account_ credit.

_`Economic protocol`
    The economic protocol is the set of rules defining valid operation_\ s and block_\ s, how the network agrees on the next block to build (the consensus algorithm),
    and how operations update the blockchain state, also called context_.

    In Tezos, the economic protocol can be upgraded without interruption or
    forking of the blockchain. This is because the procedure for an upgrade is also defined within the economic protocol, which can thus update itself.

_`Fitness` (a.k.a. score, weight)
    For each block, the consensus algotrithm can compute a score called fitness which determines the quality of the chain leading to that block.
    The shell changes the head of the chain to the valid block that has the highest fitness.

_`Height`
    See level_.

.. _def_level_mumbai:

_`Level` (a.k.a. block height)
    The position of a block_ in the blockchain, that is, the number of blocks
    since the genesis block, where the genesis block is at level 0.

_`Mempool`
    A (block or operation) metadata is a piece of data
    computed as a result of the application of the
    block or operation on an associated context_. The metadata
    consists of many pieces of information such as the operation receipts,
    rewards updates, voting period, etc.

    A block's metadata is the collections of operations metadata for all the operations included in the block (if the validation was successful).

    For a detailed metadata content check the :doc:`./rpc` under
    the prefix ``../<block_id>/metadata``.

_`Node`
    A peer in the P2P network. It maintains a local state and propagates block_\ s
    and operation_\ s.

_`Operation`
    An operation transforms the context_; this is what makes the state of the chain
    change. Operations are grouped into block_\ s; thus, the chain progresses in
    batches.
    For the different kinds of operations defined by the protocol, see `operation kinds`_.

_`Score`
    See fitness_.

_`Shell`
    The shell is a software component of the node_. It is parameterized by a
    specific `economic protocol`_. It serves as the bridge between the P2P layer
    (handling communication between nodes) and the `economic protocol`_ layer
    (handling the context_, operation_ application, scoring, etc.).

_`Weight`
    See fitness_.

Protocol
--------

.. _def_accuser_mumbai:

_`Accuser`
    When a delegate_ attempts to inject several incompatible blocks (or when it tries
    to abuse the network in another similar way), another delegate_ can make an
    accusation: show evidence of attempted abuse. The delegate_ making the accusation
    is the accuser.

    The accuser is awarded some funds from the security deposit of the accused.

    When using :ref:`Octez <octez>`, accusations are handled by the
    accuser binary.

.. _def_account_mumbai:

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
    baking and endorsing rights are computed for all the block_ levels in the
    cycle_, based on the proportion of the stake owned by each account.

    For each block_ level and block round_, there is exactly one account that is allowed to bake.

    When a block_ is created and propagated on the network, delegates that have
    `endorsing rights`_ for the matching block_ level can emit an endorsement
    operation_.
    Endorsement operation_\ s are included in the next block_.

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

.. _def_cycle_mumbai:

_`Cycle`
    A cycle is a set of consecutive blocks. E.g., cycle 12 started at block_
    level 49152 and ended at block_ level 53248.

    Cycles are used as a unit of “time” in the blockchain. For
    example, the different phases in the amendment voting procedures
    are defined based on cycles.

    The length of a cycle is a (parametric) protocol
    :ref:`constant<Constants>`, and thus might change across different
    Tezos protocols.

.. _def_delegate_mumbai:

_`Delegate`
    An `implicit account`_ to which an account_ has delegated their
    rights to participate in consensus (aka baking_ rights) and in
    governance.
    The delegate's rights are calculated based on its own tokens plus the sum of tokens
    delegated to it. Note that since ``tz4`` accounts cannot be delegates.

_`Delegation`
    An operation_ in which an account_ balance is lent to a
    delegate_. This increases the delegate_'s stake and consequently
    its baking_ rights. The delegate_ does not control the funds from
    the account_.

.. _def_double_signing_mumbai:

_`Double signing`
    The action of a baker_ signing two different blocks at the same
    level and same round is called *double baking*. Double baking
    is detrimental to the network and might be indicative of an
    attempt to double spend.  The same goes for signing two different
    *endorsements* at the same level and the same round.

    Double signing (i.e. double baking or double endorsing) is
    punished by the network: an accuser_ can provide proof of the
    double signing to be awarded part of the double signer's deposit
    -- see :ref:`Slashing<slashing_mumbai>`.

_`Failing Noop`
   The ``Failing_noop`` operation implements a *No-op*, which always
   fails at :ref:`application time<operation_validity_mumbai>`, and
   should never appear in :ref:`applied
   blocks<full_application_mumbai>`. This operation allows end-users to
   :ref:`sign arbitrary messages<failing_noop>` which have no
   computational semantics.

.. _def_fee_mumbai:

_`Fee`
   To ensure responsible use of computation resources of other
   nodes, and also to encourage active participation in the consensus
   protocol, users pay fees to bakers for including (some of) their
   operations in blocks. For example, fees are paid to a baker for
   operations such as a transaction_ or a revelation of a public key.

   Currently, only :ref:`manager operations<manager_operations_mumbai>`
   require collecting fees from its sender account_.

   See also `burn`_.

.. _def_gas_mumbai:

_`Gas`
    A measure of the number of elementary operation_\ s performed during
    the execution of a `smart contract`_. Gas is used to measure how
    much computing power is used to execute a `smart contract`_.

.. _def_implicit_account_mumbai:

_`Implicit account`
    An account_ that is linked to a public key. Contrary to a `smart
    contract`_, an `Implicit account`_ cannot include a script and it
    cannot reject incoming transactions.

    If *registered*, an `implicit account`_ can act as a delegate_.

    The address of an `implicit account`_ always starts with the
    letters `tz` followed by `1`, `2`, `3` or `4` (depending on the
    signature scheme) and finally the hash of the public key.

_`Layer 1`
    The primary blockchain i.e. the Tezos chain. Within any blockchain ecosystem, Layer 1 (L1) refers to the main chain to
    which side chains, rollups, or other protocols connect and settle to. The Layer 1 chain is deemed to be most
    secure, since it has the most value (or stake) tied to it, and be most decentralized and censorship resistant.
    However, transaction space is limited leading to low throughput and possibly high transaction costs.
    See `Layer 2`_.

_`Layer 2`
    Layer 2 (L2) includes sidechains, rollups, payment channels, etc. that batch their transactions and
    write to the `layer 1`_ chain. By processing transactions on layer 2 networks,
    greater scalability in speed and throughput can be achieved by the ecosystem overall, since the number of transactions
    the layer 1 can process directly is limited. By cementing transactions from a L2 to L1,
    the security of the L1 chain backs those operations. In Tezos there are a number of layer 2 solutions,
    including :doc:`TORUs (Transaction Optimistic Rollups) <transaction_rollups>`,
    `Smart Optimistic Rollups`_,
    validity or ZK-Rollups `Epoxy <https://research-development.nomadic-labs.com/files/cryptography.html>`_ ,
    zkChannels, and sidechains such as `Deku <https://deku.marigold.dev/>`_.

_`Michelson`
    The built-in language used by a `smart contract`_.

.. _def_minimal_stake:
.. _def_minimal_stake_mumbai:

_`Minimal stake`
    An amount of tez (e.g., 6000ꜩ) serving as a minimal amount for a
    delegate to have baking_ and voting rights in a cycle_.

_`Operation kinds`
    The main kinds of operations in the protocol are transactions (to transfer funds
    or to execute smart contracts), accusations, activations, delegations,
    endorsements and originations.

_`Originated account`
    See `smart contract`_.

.. _def_origination_mumbai:

_`Origination`
    A manager operation_ whose purpose is to create -- that
    is, to deploy -- a `smart contract`_ on the Tezos blockchain.

.. _def_round_mumbai:

_`Round`
    An attempt to reach consensus on a block at a given level.
    A round is represented by an index, starting with 0.
    Each round corresponds to a time span.
    A baker_ with baking_ rights at a given round is only allowed to bake during
    the round's corresponding time span. Baking_ outside of one's designated
    round results in an invalid block_.

_`Roll`
    deprecated; see `Minimal stake`_.

_`Smart contract`
    Account_ which is associated to a Michelson_ script. They are
    created with an explicit origination_ operation and are therefore
    sometimes called originated accounts. The address of a smart
    contract always starts with the letters ``KT1``.

_`Smart Optimistic Rollups`
    Smart optimistic rollups constitute a `layer 2`_ solution that can be used to deploy either a general-purpose polyvalent layer 2 blockchain
    (e.g., an EVM-compatible one), or an application-specific DApp.
    See :doc:`smart_rollups`.

_`Transaction`
    An operation_ to transfer tez between two accounts, or to run the code of a
    `smart contract`_.

_`Validation pass`
    An index (a natural number) associated with a particular kind of
    operations, allowing to group them into classes. Validation passes
    enable prioritizing the :ref:`validation and
    application<operation_validity_mumbai>` of certain classes of
    operations.

_`Voting period`
    Any of the ``proposal``, ``exploration``, ``cooldown``,
    ``promotion`` or ``adoption`` stages in the voting procedure when
    amending the `economic protocol`_.

_`Voting listings`
    The list calculated at the beginning of each `voting period`_ that contains
    the staking balance (in number of mutez) of each delegate_ that owns more
    than one roll_ at that moment. For each delegate_, the voting listings
    reflect the weight of the vote emitted by the delegate_ when amending the
    `economic protocol`_.
