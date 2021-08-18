..
  This is the protocol-independent part of the glossary.
  It is included by all the protocol-dependent glossaries.

_`Block`
    A block is a collection of several Operations_. The operations_ are located
    in the payload of the block which is specific to the `Economic Protocol`_.

    Along with the payload, the block includes a header which contains
    `protocol`-agnostic data. It consists of generic information such as the
    block predecessor's hash and the block's timestamp.

    _`Metadata`
    A metadata, or block's metadata, is a piece of data associated to
    a block. It is computed as a result of the application of the
    block's Operations_ on its associated Context_. The metadata
    consists in many information such as the operation receipts,
    rewards updates, voting period, etc.

    For a detailed metadata content check the :ref:`rpc_index` under
    the prefix ``../<block_id>/metadata``.

_`Context`
    The state of the blockchain. The context is defined by the
    `Economic Protocol`_ and typically includes information such as
    “this account_ is credited with this many tez” and “this is the
    code for that `smart contract`_.”

    The Context is modified by Operations_. For example, an
    operation_ can transfer tez from one account_ to another, which modifies the
    part of the context that tracks account_ credit.

_`Economic protocol`
    The economic protocol is the application that runs on top of the blockchain
    proper. It defines a Context_ (the state of the application), some
    Operations_ (how the state evolves).

    In Tezos, the economic protocol can be upgraded without interruption or
    forking of the blockchain. The procedure for an upgrade is defined within
    the economic protocol itself so it can be upgraded as well.

_`Fitness`
    See score_.

_`Mempool`
    A pool (set) of operations_ maintained by a node_ and not yet included in a block_.

_`Node`
    A peer in the P2P network. It maintains a local state and propagates blocks
    and operations_.

_`Operation`
    Operations_ transform the context_, this is what makes the state of the chain
    change. Operations_ are grouped into blocks so that the chain progresses in
    batches.

_`Score` (a.k.a. Fitness, a.k.a. Weight)
    The score is a metric used to compare contexts. For example, when several
    blocks claim to be heads of the chain, their context_'s scores are compared.
    The highest scoring block_ is selected as the head of the chain.

_`Shell`
    The shell is a software component of the node_. It is parameterized by a
    specific `economic protocol`_. It serves as the bridge between the P2P layer
    (handling communication between nodes) and the `economic protocol`_ layer
    (handling the context_, operation_ application, scoring, etc.).

_`Weight`
    See score_.
