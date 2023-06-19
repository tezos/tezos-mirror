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

.. _def_context:
.. _def_context_nairobi:

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

.. _def_level:
.. _def_level_nairobi:

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

.. _def_accuser:
.. _def_accuser_nairobi:

_`Accuser`
    When a delegate_ attempts `double signing`_ (or when it tries
    to abuse the network in another similar way), another delegate_ can make an
    accusation, by providing evidence of the offense. The delegate_ injecting the accusation in a newly baked block is called the accuser.\

    The accuser is awarded some funds from the security deposit of the accused.

    When using :ref:`Octez <octez>`, accusation operations are emitted by the
    accuser daemon. Note that this daemon is not associated to a delegate: accusation operations are anonymous, and any delegate can include them in a block.

.. _def_account:
.. _def_account_nairobi:

_`Account`
    An account is an address managed by the protocol.
    In the context_, each account is associated with a balance (an amount of
    tez available).

    An account can be either an `originated account`_ or an `implicit account`_.

_`Baker`
    When a delegate_ creates a new block_, it is called the baker of this block.
    `Baking rights`_ are distributed to different delegates based on their
    available stake_. Only a delegate with baking rights
    is allowed to bake.
    The baker selects transactions from the mempool_ to be included in the block it bakes.

    When using :ref:`Octez <octez>`, baking and other consensus actions are handled by the baker
    daemon, on behalf of one or more delegate_ accounts.
    By extension, a baker designates the owner of such a delegate account, typically running the baker daemon on its behalf.

_`Baking`
    The act of creating a new block_ by a baker_.

_`Baking rights`
    Baking_/endorsing_ a block_ can only be done by a delegate_ who holds the
    baking/endorsing right for that block level_ and round_. At the start of a cycle_,
    baking and endorsing rights are computed for all the block_ levels and rounds in the
    cycle_, based on the proportion of the stake_ of each delegate_.

    For each block_ level and round_, there is exactly one account that is allowed to bake, but several accounts are allowed to endorse.

_`Burn`
    To ensure responsible use of the storage space on the public blockchain,
    there are some costs charged to users for consuming storage. These
    costs are burnt (i.e., the amount of tez is destroyed). For example,
    a per-byte storage cost is burnt for increasing the storage space of a
    smart contract; a fixed amount is burnt for allocating a new contract
    (which consumes space by storing its address on the blockchain).

    See also `fee`_.

_`Constant`
    Protocols are parameterized by several parameters called protocol constants, which may vary from one protocol to another or from one network to another.

_`Contract`
    See account_.

.. _def_cycle:
.. _def_cycle_nairobi:

_`Cycle`
    A cycle is a sequence of consecutive block_\ s of fixed length (given by a protocol constant_). E.g., cycle 12 started at block
    level 49152 and ended at block_ level 53248.

    Cycles are used as a unit of “time” in the block_ chain. For example, the
    different phases in the amendment voting procedures are defined based on
    numbers of cycles.

    The length of a cycle is a (parametric) protocol
    constant_, and thus might change across different
    Tezos protocols.

.. _def_delegate:
.. _def_delegate_nairobi:

_`Delegate`
    An `implicit account`_ that can participate in consensus and in governance.
    Actual participation is under further provisions, like having a `minimal stake`_.
    An implicit account becomes a delegate by registering as such.
    Through delegation_, other accounts can delegate their rights to a delegate account.
    The delegate's rights are calculated based on its stake_.
    Note that ``tz4`` accounts cannot be delegates.

_`Delegation`
    An operation_ in which an account_ designates a
    delegate_. The delegating account's balance increases the delegate_'s stake_ and consequently
    its `baking rights`_ and `endorsing rights`_. However, the delegate_ does not control the funds of
    the delegating account_, e.g., it can not spend them.

.. _def_double_signing:
.. _def_double_signing_nairobi:

_`Double signing`
    The situation when a baker_ signs two different block_\ s at the same level and same round,
    is called double baking. Double baking is detrimental to the network and might be
    indicative of an attempt to double spend.
    The same goes for signing two different endorsements at the same level and the same round.
    As such, double signing (i.e., double baking or double endorsing) is punished by the
    network: an accuser_ can provide proof of the double signing to be awarded
    part of the double signer's deposit -- see :ref:`Slashing<slashing_nairobi>`.

_`Failing Noop`
    The ``Failing_noop`` operation implements a *No-op*, which always
    fails at :ref:`application time<operation_validity_nairobi>`, and
    should never appear in :ref:`applied
    blocks<full_application_nairobi>`. This operation allows end-users to
    :ref:`sign arbitrary messages<failing_noop>` which have no
    computational semantics.

_`Endorsing`
    When a block_ is created and propagated on the network, delegates that have
    `endorsing rights`_ for the matching block level_ and round_ can emit an endorsement operation_.
    Endorsement operations are included in the next block_.

_`Endorsing rights`
    See `baking rights`_.

.. _def_fee:
.. _def_fee_nairobi:

_`Fee`
    To ensure responsible use of computation resources of other nodes, and also to encourage active participation in the consensus protocol,
    users pay fees to bakers for including their operation_\ s in block_\ s.
    For example, fees are paid to a baker for operations such as a transaction_ or a revelation of a public key.

    Currently, only :ref:`manager operations<manager_operations_nairobi>`
    require collecting fees from its sender account_.

    See also `burn`_.

.. _def_gas:
.. _def_gas_nairobi:

_`Gas`
    A measure of the number of elementary steps performed during
    the execution of a `smart contract`_. Gas is used to measure how
    much computing power is used to execute a `smart contract`_.

.. _def_implicit_account:
.. _def_implicit_account_nairobi:

_`Implicit account`
    An account_ that is linked to a public key. Contrary to a `smart
    contract`_, an implicit account cannot include a script and it
    cannot reject incoming transactions.

    If *registered*, an `implicit account`_ can act as a delegate_.

    The address of an implicit account always starts with the
    letters `tz` followed by `1`, `2`, `3`, or `4` (depending on the
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
    including :doc:`Smart Optimistic Rollups <smart_rollups>`,
    validity or ZK-Rollups `Epoxy <https://research-development.nomadic-labs.com/files/cryptography.html>`_ ,
    zkChannels, and sidechains such as `Deku <https://deku.marigold.dev/>`_.

_`Michelson`
    The built-in language used by a `smart contract`_.

.. _def_minimal_stake:
.. _def_minimal_stake_nairobi:

_`Minimal stake`
    An amount of tez (e.g., 6000ꜩ) serving as a minimal amount for a
    delegate to have `baking rights`_ and voting rights in a cycle_.

_`Operation kinds`
    The main kinds of operations in the protocol are transactions (to transfer funds
    or to execute smart contracts), accusations, activations, delegations,
    endorsements, and originations.
    For the full list of operations, see :doc:`./blocks_ops`.

_`Originated account`
    See `smart contract`_.

.. _def_origination:
.. _def_origination_nairobi:

_`Origination`
    A manager operation_ whose purpose is to create -- that
    is, to deploy -- a `smart contract`_ on the Tezos blockchain.

.. _def_round:
.. _def_round_nairobi:

_`Round`
    An attempt to reach consensus on a block at a given level.
    A round is represented by an index, starting with 0.
    Each round corresponds to a time span.
    A baker_ with `baking rights`_ at a given round is only allowed to bake during
    the round's corresponding time span. Baking_ outside of one's designated
    round results in an invalid block_.

_`Roll`
    deprecated; see `minimal stake`_.

_`Smart contract`
    Account_ which is associated to a Michelson_ script.
    They are created with an
    explicit origination_ operation and are therefore sometimes called
    originated accounts. The address of a smart contract always starts
    with the letters ``KT1``.

_`Smart Optimistic Rollups`
    Smart optimistic rollups constitute a `layer 2`_ solution that can be used to deploy either a general-purpose polyvalent layer 2 blockchain
    (e.g., an EVM-compatible one), or an application-specific DApp.
    See :doc:`smart_rollups`.

_`Stake`
    The amount of tokens that determines a delegate_'s weight in the
    governance process and in the selection of its baking and
    `endorsing rights`_. A delegate's stake is usually given by the
    delegate's own tokens plus the sum of tokens delegated to
    it. However, there are cases when this is not the case, see
    :ref:`here<active_stake_nairobi>` for details.

_`Transaction`
    An operation_ to transfer tez between two accounts, or to run the code of a
    `smart contract`_.

_`Validation pass`
    An index (a natural number) associated with a particular kind of
    operations, allowing to group them into classes. Validation passes
    enable prioritizing the :ref:`validation and
    application<operation_validity_nairobi>` of certain classes of
    operations.

_`Voting period`
    Any of the ``proposal``, ``exploration``, ``cooldown``,
    ``promotion`` or ``adoption`` stages in the voting procedure when
    amending the `economic protocol`_.

_`Voting listings`
    The list calculated at the beginning of each `voting period`_ that contains
    the staking balance (in number of mutez) of each delegate_ that owns more
    than the `minimal stake`_ at that moment. For each delegate_, the voting listings
    reflect the weight of the vote emitted by the delegate_ when amending the
    `economic protocol`_.
