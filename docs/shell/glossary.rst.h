..
  This is the protocol-independent part of the glossary.
  It is included by all the protocol-dependent glossaries.

_`Block`
    The Tezos blockchain is a linked list of blocks (or actually, a tree when several competing branches exist).
    Blocks conceptually contain a header and a list of Operations_,
    which are specific to the `economic protocol`_.

    The header itself decomposes into a shell header (common to all protocols) and a protocol-specific header.
    The shell header contains `protocol`-agnostic data such as the
    block predecessor's hash and the block's timestamp.

_`Context`
    The state of the blockchain. The context is defined by the
    `economic protocol`_ and typically includes information such as
    “this account_ is credited with this many tez” and “this is the
    code for that `smart contract`_.”

    The context is modified by operations_. For example, an
    operation_ can transfer tez from one account_ to another, which modifies the
    part of the context that tracks account_ credit.

_`Economic protocol`
    The economic protocol is the application that runs on top of the blockchain
    proper. It defines a context_ (the state of the application), some
    operations_ (how the state evolves).

    In Tezos, the economic protocol can be upgraded without interruption or
    forking of the blockchain. The procedure for an upgrade is defined within
    the economic protocol itself so it can be upgraded as well.

_`Fitness` (a.k.a. score, a.k.a. weight)
    To each block, we associate a fitness which determines the quality of the chain leading to that block.
    This measure is computed by the consensus protocol.
    The shell changes the head of the chain to the valid block that has the highest fitness.

_`Level`
    The position of a block in the chain, that is, the number of blocks
    since the genesis block, where the genesis block is at level 0.

_`Mempool`
    A pool (set) of operations_ maintained by a node_ and not yet included in a block_.

_`Metadata`
    A metadata, or operation's metadata, is a piece of data associated to
    an Operation_. It is computed as a result of the application of the
    operation included in a block_ on its associated context_. The metadata
    consists in many information such as the operation receipts,
    rewards updates, voting period, etc.

    A block's metadata is the collections of operations metadata for all the operations included in the block (if the validation was successful).

    For a detailed metadata content check the :ref:`rpc_index` under
    the prefix ``../<block_id>/metadata``.

_`Node`
    A peer in the P2P network. It maintains a local state and propagates blocks
    and operations_.

_`Operation`
    Operations_ transform the context_, this is what makes the state of the chain
    change. Operations_ are grouped into blocks so that the chain progresses in
    batches.

_`Score`
    See fitness_.

_`Shell`
    The shell is a software component of the node_. It is parameterized by a
    specific `economic protocol`_. It serves as the bridge between the P2P layer
    (handling communication between nodes) and the `economic protocol`_ layer
    (handling the context_, operation_ application, scoring, etc.).

_`Weight`
    See fitness_.
