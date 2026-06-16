Glossary
========

This glossary is divided in two sections:

- The first section contains Tezos generic terms (i.e., which are not related to a particular protocol), such as Tezos_ or block_.
- The second section contains terms defined by the `economic protocol`_, such as Tez_/XTZ_, account_, `smart contract`_, or delegate_.
  The list of terms in this section and/or their definitions may be different for other protocol versions.

Tezos terms
-----------

_`Block`
    The Tezos blockchain is a linked list of blocks (or actually, a tree when several competing branches exist).
    Blocks conceptually contain a header and a list of operation_\ s,
    which are specific to the `economic protocol`_.

    The header itself decomposes into a :ref:`shell header<shell_header>` (common to all Tezos economic protocols), and a protocol-specific header.
    The shell header contains protocol-agnostic data such as the predecessor's block hash and the block's timestamp.

.. _def_context:
.. _def_context_tallinn:

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
.. _def_level_tallinn:

_`Level` (a.k.a. block height)
    The position of a block_ in the blockchain, that is, the number of blocks
    since the genesis block, where the genesis block is at level 0.

_`Mempool`
   A pool (set) of operation_\ s maintained by a node_ and not yet included in a block_.

.. _def_metadata:
.. _def_metadata_tallinn:

_`Metadata`
    A (block or operation) metadata is a piece of data
    computed as a result of the application of the
    block or operation on an associated context_. The metadata
    consists of many pieces of information such as the operation receipts,
    rewards updates, voting period, etc.

    A block's metadata is the collections of operations metadata for all the operations included in the block (if the validation was successful).

    For a detailed metadata content check the :doc:`./rpc` under
    the prefix ``../<block_id>/metadata``.

_`Node`
    A peer in a Tezos P2P network.
    The term "node" can refer to a few different kinds of nodes, including Tezos Layer 1 nodes, `DAL node`_\ s, and `Smart Rollup`_ nodes.
    The context should help to disambiguate between these, but in most cases, the term "node" refers to a Layer 1 node.
    Layer 1 nodes maintain a local state and propagates block_\ s and operation_\ s.

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

_`Tezos`
    Used either as a noun or an adjective to designate:

    * An open-source project and software (as in, "contributing to the Tezos protocol")
    * A peer-to-peer network of nodes maintaining a blockchain (as in "a Tezos node")
    * The specific Tezos chain with the most economic relevance (as in "the Tezos chain"). In particular, the chain whose millionth block had hash `BKtC4QCWoF73kxLj773vFpQuuwrnye6PS7T1aM3XEPvFXiQbNu7 <https://tzkt.io/BKtC4QCWoF73kxLj773vFpQuuwrnye6PS7T1aM3XEPvFXiQbNu7>`__

_`Weight`
    See fitness_.

Protocol terms
--------------

.. _def_accuser:
.. _def_accuser_tallinn:

_`Accuser`
    When a delegate_ attempts `double signing`_ (or when it tries
    to abuse the network in another similar way), another delegate_ can make an
    accusation, by providing evidence of the offense. The delegate_ injecting the accusation in a newly baked block is called the accuser.\

    The accuser is awarded some funds from the security deposit of the accused.

    When using :ref:`Octez <octez>`, accusation operations are emitted by the
    accuser daemon. Note that this daemon is not associated to a delegate: accusation operations are anonymous, and any delegate can include them in a block.

.. _def_account:
.. _def_account_tallinn:

_`Account`
    An account is an address managed by the protocol.
    In the context_, each account is associated with a balance (an amount of
    tez available).

    An account can be either a `user account`_ or a `smart contract`_.

_`Attestation lag`
    A delay of a certain number of blocks that gives DAL nodes time to verify that DAL data is available.
    Attesters (via their DAL nodes) include their DAL attestations in their attestation operations immediately after the attestation lag has passed.

_`Attestation quorum`
    As described in :doc:`The consensus algorithm<consensus>`, the minimal number of attestations that are needed to consider a block as pre-attested or attested.

_`Attestation threshold`
    In the DAL, the minimum percentage of DAL shard_\ s that must be attested before the data is considered available to Smart Rollups.

_`Attesting`
    The process of asserting that some other operation is valid.
    When a block_ is created and propagated on the network, delegates that have
    `attesting rights`_ for the matching block level_ and round_ can emit an attestation operation_.
    Attestation operations are included in the next block_.
    Beyond serving in the L1 consensus protocol for attesting block_\ s, attestation operations provide specific fields used in the DAL_ for attesting available shard_\ s.

_`Attesting rights`
    See `baking rights`_.

_`Baker`
    When a delegate_ creates a new block_, it is called the baker of this block.
    `Baking rights`_ are distributed to different delegates based on their
    `baking power`_. Only a delegate with baking rights
    is allowed to bake.
    The baker selects transactions from the mempool_ to be included in the block it bakes.

    When using :ref:`Octez <octez>`, baking and other consensus actions are handled by the baker
    daemon, on behalf of one or more delegate_ accounts.
    By extension, a baker designates the owner of such a delegate account, typically running the baker daemon on its behalf, participating in governance and in the DAL.

_`Baking`
    The act of creating a new block_ by a baker_.

_`Baking power`
    The amount of tokens that determines a delegate_'s weight
    in the selection of its baking and
    `attesting rights`_. A delegate's baking power is computed from the
    delegate's own tokens and the sums of tokens delegated and staked to
    it (where staked tokens weigh more than tokens just delegated). See
    :doc:`./baking_power` for details.

_`Baking rights`
    Baking_/attesting_ a block_ can only be done by a delegate_ who holds the
    baking/attesting right for that block level_ and round_. At the start of a cycle_,
    baking and attesting rights are computed for all the block_ levels and rounds in the
    cycle_, based on the `baking power`_ of each delegate_.

    For each block_ level and round_, there is exactly one account that is allowed to bake, but several accounts are allowed to attest.

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

.. _def_cycle:
.. _def_cycle_tallinn:

_`Cycle`
    A cycle is a sequence of consecutive block_\ s of fixed length (given by a protocol constant_). E.g., cycle 12 started at block
    level 49152 and ended at block_ level 53248.

    Cycles are used as a unit of “time” in the block_ chain. For example, the
    different phases in the amendment voting procedures are defined based on
    numbers of cycles.

    The length of a cycle is a (parametric) protocol
    constant_, and thus might change across different
    Tezos protocols.

_`DAL`
    See `Data Availability Layer`_

_`DAL node`
    DAL nodes (instances of the ``octez-dal-node`` binary) are responsible for distributing the data that users submit to the DAL.

_`DAL slot`
    Each block has a certain number of slots to which Data Availability Layer users can post raw data (also called blobs, for binary large objects) to distribute via the DAL.

_`Data Availability Layer`
    The Data Availability Layer (DAL) is a companion peer-to-peer network for Tezos that distributes data to Smart Rollups.
    See :doc:`../shell/dal`.

.. _def_delegate:
.. _def_delegate_tallinn:

_`Delegate`
    A `user account`_ that can participate in consensus, in governance, and in the DAL.
    Actual participation is under further provisions, like having a `minimal stake`_.
    A user account becomes a delegate by registering as such.
    Through delegation_, other accounts can delegate their rights to a delegate account.

_`Delegation`
    An operation_ in which an account_ designates a
    delegate_. The delegating account's balance increases the delegate_'s `baking power`_ and consequently
    its `baking rights`_ and `attesting rights`_; it also increases its `voting power`_. However, the delegate_ does not control the funds of
    the delegating account_, e.g., it can not spend them.

.. _def_double_signing:
.. _def_double_signing_tallinn:

_`Double signing`
    The situation when a baker_ signs two different block_\ s at the same level and same round,
    is called double baking. Double baking is detrimental to the network and might be
    indicative of an attempt to double spend.
    The same goes for signing two different attestations at the same level and the same round.
    As such, double signing (i.e., double baking or double attesting) is punished by the
    network: an accuser_ can provide proof of the double signing to be awarded
    part of the double signer's deposit -- see :ref:`Slashing<slashing_tallinn>`.

_`Failing Noop`
    The ``Failing_noop`` operation implements a *No-op*, which always
    fails at :ref:`application time<operation_validity_tallinn>`, and
    should never appear in :ref:`applied
    blocks<full_application_tallinn>`. This operation allows end-users to
    :ref:`sign arbitrary messages<failing_noop_tallinn>` which have no
    computational semantics.

.. _def_fee:
.. _def_fee_tallinn:

_`Fee`
    To ensure responsible use of computation resources of other nodes, and also to encourage active participation in the consensus protocol,
    users pay fees to bakers for including their operation_\ s in block_\ s.
    For example, fees are paid to a baker for operations such as a transaction_ or a revelation of a public key.

    Currently, only :ref:`manager operations<manager_operations_tallinn>`
    require collecting fees from its sender account_.

    See also `burn`_.

.. _def_gas:
.. _def_gas_tallinn:

_`Gas`
    A measure of the number of elementary steps performed during
    the execution of a `smart contract`_. Gas is used to measure how
    much computing power is used to execute a `smart contract`_.

_`Implicit account`
    See `user account`_.

_`Layer 1`
    The primary blockchain i.e. the Tezos chain. Within any blockchain ecosystem, Layer 1 (L1) refers to the main chain to
    which side chains, rollups, or other protocols connect and settle to. The Layer 1 chain is deemed to be most
    secure, since it has the most value (or stake) tied to it, and be most decentralized and censorship resistant.
    However, transaction space is limited leading to low throughput and possibly high transaction costs.
    See `Layer 2`_.

_`Layer 2`
    Layer 2 (L2) includes sidechains, rollups, payment channels, etc. that batch their transactions and
    write to the `Layer 1`_ chain. By processing transactions on Layer 2 networks,
    greater scalability in speed and throughput can be achieved by the ecosystem overall, since the number of transactions
    the Layer 1 can process directly is limited. By cementing transactions from a L2 to L1,
    the security of the L1 chain backs those operations. Currently, Layer 2 solutions on Tezos are built as `smart rollup`_\ s.

_`Michelson`
    The built-in language used by a `smart contract`_.

.. _def_minimal_stake:
.. _def_minimal_stake_tallinn:

_`Minimal stake`
    An amount of tez (e.g., 6000ꜩ) serving as a minimal amount for a
    delegate to have `baking rights`_ and voting rights in a cycle_.

_`Operation kinds`
    The main kinds of operations in the protocol are transactions (to transfer funds
    or to execute smart contracts), accusations, activations, delegations,
    attestations, and originations.
    For the full list of operations, see :doc:`./blocks_ops`.

_`Originated account`
    See `smart contract`_.

.. _def_origination:
.. _def_origination_tallinn:

_`Origination`
    A manager operation_ whose purpose is to create -- that
    is, to deploy -- a `smart contract`_ on the Tezos blockchain.

_`Page`
    A portion of the raw (unexpanded) data in a `DAL slot`_ of a fixed size that is small enough to fit in a Layer 1 operation.
    The DAL splits the raw data in each slot into pages that can be requested individually so that for example refutation games can run properly.

_`PVM`
   A PVM (Proof-generating Virtual Machine) is a reference
   implementation for a device on top of which a `smart rollup`_ can be
   executed. This reference implementation is part of the `economic
   protocol`_ and is the unique source of truth regarding the semantics
   of rollups. The PVM is able to produce proofs enforcing this truth.
   This ability is used during the final step of a `refutation game`_.

_`Redundancy factor`
    The amount of redundancy that the protocol uses when expanding and splitting DAL data into shard_\ s.
    The higher the redundancy factor, the fewer shards are needed to reconstruct the initial data.

_`Refutation game`
   A process by which the `economic protocol`_ solves a conflict between two
   `rollup committer`_\ s.
   Note that the refutation mechanism used in Tezos `smart rollup`_\ s corresponds to the notion of `fraud proofs <https://www.binance.com/en/academy/glossary/fraud-proof>`__ used in other blockchain/Layer 2 ecosystems.

_`Refutation period`
   When the first `rollup commitment`_ for a `rollup commitment period`_ is published, a refutation
   period of two weeks starts to allow this commitment to be challenged.

_`Roll`
    deprecated; see `minimal stake`_.

_`Rollup commitment`
   A claim that the interpretation of all `rollup inbox`_ messages
   published during a given period, and applied on the state of
   a parent rollup commitment, led to a given new state by performing a given
   number of execution steps of the `PVM`_.

_`Rollup commitment period`
   A period of roughly 15 minutes during which all `rollup inbox`_
   messages must be processed by the `rollup node`_ state to compute a
   `rollup commitment`_. A commitment must be published for each commitment
   period.

_`Rollup committer`
   A `user account`_ that has published and made a deposit on a
   `rollup commitment`_.

_`Rollup inbox`
   A sequence of messages from the Layer 1 to all the `smart rollup`_\ s.
   The contents of the inbox are determined by the consensus of the
   `economic protocol`_.

_`Rollup node`
   A daemon required for deploying and operating `smart rollup`_\ s.
   The rollup node is responsible for making the rollup progress by publishing `rollup commitment`_\ s and by playing `refutation game`_\ s.

_`Rollup outbox`
   A sequence of messages from a `smart rollup`_ to the Layer 1.
   Messages are `smart contract`_ calls, potentially containing tickets.
   These calls can be triggered only when the related `rollup commitment`_ is
   cemented (hence, at least two weeks after the actual execution of
   the operation).

.. _def_round:
.. _def_round_tallinn:

_`Round`
    An attempt to reach consensus on a block at a given level.
    A round is represented by an index, starting with 0.
    Each round corresponds to a time span.
    A baker_ with `baking rights`_ at a given round is only allowed to bake during
    the round's corresponding time span. Baking_ outside of one's designated
    round results in an invalid block_.

_`Shard`
    Raw data aimed to published over the DAL is first expanded based on a redundancy factor and then split into shards.
    The DAL distributes these shards to DAL nodes.
    Attesters are assigned shards to download (via DAL nodes) and attest.
    Thanks to redundancy, not every shard needs to be attested for the full data to be considered available.

_`Smart contract`
    Account_ which is associated to a Michelson_ script.
    They are created with an
    explicit origination_ operation and are therefore sometimes called
    originated accounts. The address of a smart contract always starts
    with the letters ``KT1``.

_`Smart Rollup`
    Smart rollups constitute a `Layer 2`_ solution that can be used to deploy either a general-purpose polyvalent Layer 2 blockchain
    (e.g., an EVM-compatible one), or an application-specific DApp.
    See :doc:`smart_rollups`.

.. _def_staker:
.. _def_staker_tallinn:

_`Staker`
    A `user account`_ that made a security deposit.
    The user account must have set a delegate.
    The security deposit accrues to the stake of the user account's delegate and is
    subject to slashing in case the delegate misbehaves -- see :ref:`Slashing<slashing_tallinn>`.

_`Tenderbake`
   The algorithm that Tezos uses to create consensus from bakers' proposals and attestations of blocks.

_`Tez`
    A unit of the cryptocurrency native to a Tezos_ chain, such as in "I sent you 2 tez." Tez is invariable. It is not capitalized except at the beginning of a sentence or when you would otherwise capitalize a noun.
    See also XTZ_.

_`Transaction`
    An operation_ to transfer tez between two accounts, or to run the code of a
    `smart contract`_.

_`Trap`
   A DAL shard_ that is treated specially by the protocol for a particular attester to ensure that attesters are honestly downloading and attesting to data.
   If a DAL attester attests to a trap, that attester will not receive DAL rewards.

.. _def_user_account:
.. _def_user_account_tallinn:

_`User account`
    An account_ that is linked to a public key. Contrary to a `smart
    contract`_, a user account cannot include a script and it
    cannot reject incoming transactions.
    User accounts are sometimes called "implicit accounts".

    If *registered*, a user account can act as a delegate_.

    The address of a user account always starts with the
    letters ``tz`` followed by ``1``, ``2``, ``3``, or ``4`` (depending on the
    signature scheme) and finally the hash of the public key.
    See :doc:`./accounts` for a more detailed explanation on addresses.

_`Validation pass`
    An index (a natural number) associated with a particular kind of
    operations, allowing to group them into classes. Validation passes
    enable prioritizing the :ref:`validation and
    application<operation_validity_tallinn>` of certain classes of
    operations.

_`Voting period`
    Any of the ``proposal``, ``exploration``, ``cooldown``,
    ``promotion`` or ``adoption`` stages in the voting procedure when
    amending the `economic protocol`_.

_`Voting power`
    The amount of tokens that determines a delegate_'s weight in the
    voting process. A delegate's voting power is computed from the
    delegate's own tokens and the sum of tokens delegated to
    it. See :ref:`voting_power_tallinn` for details.

_`Voting listings`
    The list calculated at the beginning of each `voting period`_ that contains
    the staking balance (in number of mutez) of each delegate_ that owns more
    than the `minimal stake`_ at that moment. For each delegate_, the voting listings
    reflect the weight of the vote emitted by the delegate_ when amending the
    `economic protocol`_.

_`XTZ`
    XTZ, tez, or ꜩ (``\ua729``, "Latin small letter tz") is the native currency of Tezos.

    "XTZ" is an ISO-4217-compatible code for representing tez on the most economically relevant Tezos chain. Unless there is a very specific reason to use an ISO code for it, the term tez is preferred. Situations where the ISO code might be useful typically involve accounting systems, exchange rates with other currencies, and anything that might need some sort of standardized code.
