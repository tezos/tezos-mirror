..
  This is the protocol-independent part of the glossary.
  It is included by all the protocol-dependent glossaries.

.. _blocks:

_`Block`
    The Tezos blockchain is a linked list of blocks (or actually, a tree when several competing branches exist).
    Blocks conceptually contain a header and a list of operations_,
    which are specific to the `economic protocol`_.

    The header itself decomposes into a :ref:`shell header<shell_header>` (common to all Tezos economic protocols), and a protocol-specific header.
    The shell header contains protocol-agnostic data such as the predecessor's block hash and the block's timestamp.

_`Context`
    The state of the blockchain. The context is defined by the
    `economic protocol`_ and typically includes information such as
    “this account_ is credited with this many tez” and “this is the
    code for that `smart contract`_.”

    The context is modified by operations_. For example, an
    operation_ can transfer tez from one account_ to another, which modifies the
    part of the context that tracks account_ credit.

_`Economic protocol`
    The economic protocol is the set of rules defining valid operations_ and blocks_, how the network agrees on the next block to build (the consensus algorithm),
    and how operations update the blockchain state, also called context_.

    In Tezos, the economic protocol can be upgraded without interruption or
    forking of the blockchain. This is because the procedure for an upgrade is also defined within the economic protocol, which can thus update itself.

_`Fitness` (a.k.a. score, weight)
    For each block, the consensus algotrithm can compute a score called fitness which determines the quality of the chain leading to that block.
    The shell changes the head of the chain to the valid block that has the highest fitness.

_`Height`
    See level_.

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
    A peer in the P2P network. It maintains a local state and propagates blocks_
    and operations_.

.. _operations:

_`Operation`
    An operation transforms the context_; this is what makes the state of the chain
    change. Operations are grouped into blocks_; thus, the chain progresses in
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
