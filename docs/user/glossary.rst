Glossary
========

This glossary is divided in two sections, the first one concerns Tezos, and
the second one concerns the Alpha protocol, which is the current
`economic protocol`_.

Tezos
-----

_`Context`
    The state of the blockchain. The context is defined by the
    `Economic Protocol`_ and typically includes information such as “this
    account_ is credited with this many tez” and “this is the code for that smart
    contract_.”

    The Context is modified by Operations_. For example, an
    operation_ can transfer tez from one account_ to another, which modifies the
    part of the context that tracks account_ credit.

_`Block`
    A block is a collection of several Operations_. The operations_ are located
    in the payload of the block which is specific to the `Economic Protocol`_.

    Along with the payload, the block includes a header which contains metadata.
    The metadata is agnostic to the `economic protocol`_. It consists of generic
    information such as the block predecessor, a time stamp, etc.

_`Economic protocol`
    The economic protocol is the application that runs on top of the blockchain
    proper. It defines a Context_ (the state of the application), some
    Operations_ (how the state evolves).

    In Tezos, the economic protocol can be upgraded without interruption or
    forking of the blockchain. The procedure for an upgrade is defined within
    the economic protocol itself so it can be upgraded as well.

_`Fitness`
    See score_.

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

Protocol alpha
--------------

_`Accuser`
    When a node_ attempts to inject several incompatible blocks (or when it tries
    to abuse the network in another similar way), another node_ can make an
    accusation: show evidence of attempted abuse. The node_ making the accusation
    is the accuser.

    The accuser is awarded some funds from the baking_ deposit of the accused.

    Using the tools provided by Nomadic Labs, accusation is handled by a
    separate binary.

_`Account`
    An account is a unique identifier within protocol alpha. There are different
    kinds of accounts (see `Originated account`_ and `Implicit account`_).

    In the Context_, each account is associated with a balance (an amount of
    tez available), a manager_ (another account that manages this one; possibly
    itself), and other information.

_`Baker`
    When a node_ creates a new block_, it is the baker of this block_.
    Baking_ rights are distributed to different accounts based on their
    available balance. Only a node_ that handles an account_ with baking_ rights
    is allowed to bake; blocks created by another node_ are invalid.

    Using the tools provided by Nomadic Labs, baking_ is handled by a
    separate binary.

_`Baking`/_`Endorsement rights`
    A delegate_ is allowed to bake/endorse a block_ if he holds the
    baking/endorsement right for that block_. At the start of a Cycle_,
    baking and endorsement rights are computed for all the block_ heights in the
    cycle_, based on the proportion of Rolls owned by each accounts.

    For each block_ height, there are several accounts that are allowed to bake.
    These different accounts are given different Priorities.

    For each block_ height, there are several accounts that are allowed to
    endorse. There can be multiple endorsements per block_. Endorsements increase
    the Score_ of the block_.

_`Contract`
    `Originated account`_ which is associated to a Michelson_ script.
    Contracts are sometimes referred to as smart contracts.

_`Cycle`
    A cycle is a set of consecutive blocks. E.g., cycle 12 started at block_
    height 49152 and ended at block_ height 53248.

    Cycles are used as a unit of “time” in the block_ chain. For example, the
    different phases in the amendment voting procedures are defined based on
    cycles.

_`Delegate`
    An `Implicit account`_ to which an account_ has delegated their baking_ and
    `endorsement rights`_. The Baking_ rights and `Endorsement rights`_ are
    calculated based on the total balance of tez that an account_ has been
    delegated to.

_`Delegation`
    An operation_ in which an `originated account`_'s manager_ lends its account_
    balance to a delegate_. This increases the delegate_'s rolls and consequently
    its Baking_ rights. The delegate_ does not control the funds from the
    account_.

_`Double baking`
    When a baker_ signs two different blocks at the same height, it is called
    double baking. Double baking is detrimental to the network and might be
    indicative of an attempt to double spend. As such, it is punished by the
    network: an accuser_ can provide proof of the double baking to be awarded
    part of the baker_'s deposit.

_`Endorser`
    When a block_ is created and propagated on the network, nodes that have
    `Endorsement rights`_ for the matching block_ height can emit an endorsement
    operation_. The accounts that emit the block_ are the endorsers of the block_.
    The endorsement operations_ can be included in the next block_ to increase
    the block_'s Score_.

    Using the tools provided by Nomadic Labs, endorsement is handled by a
    separate binary.

_`Gas`
    A measure of the number of elementary operations_ performed during the
    execution of a contract_. Gas is used to measure how much computing power is
    used to execute a contract_.

_`Implicit account` / _`Manager`
    An account_ that is linked to a public key. An `implicit account`_ cannot
    include a Contract_ and cannot be delegated. An implicit contract can be
    set as the manager for other accounts, in which case it can perform
    operations_ on these accounts.

_`Michelson`
    The built-in language used in smart contracts.

_`Operations`
    In protocol Alpha, the main operations are transactions (to transfer funds
    or to execute contracts), accusations, activations, delegations,
    endorsements, originations and transactions.

_`Originated account`
    An account_ that can contain a contract_ or be delegated. They are
    created with an explicit origination_ operation.

_`Origination`
    An operation_ to create an `originated account`_.

_`Liquidity`
    A high-level programming language that compiles to Michelson_.

_`Priority`
    A rank of different baking_ rights. Each rank corresponds to a time span. A
    baker_ with baking_ rights at a given priority is only allowed to bake during
    the priority's corresponding time span. Baking_ outside of one's designated
    priority, results in an invalid block_.

_`Roll`
    An amount of tez (e.g., 10000ꜩ) serving as a unit to determine delegates'
    baking_ rights in a cycle_. A delegate_ with twice as many rolls as another
    will be given twice as many rights to bake.

_`Transaction`
    An operation_ to transfer tez between two accounts, or to run the code of a
    contract_.

