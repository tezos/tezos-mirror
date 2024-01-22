=====================
Blocks and Operations
=====================

The content of a Tezos block is made up of operations, which implement
and reify different functionalities provided by a Tezos economic
protocol: from reaching consensus on the state of the Tezos
blockchain, to performing smart contract calls and transactions. Each
Tezos economic protocol can specify different kinds of operations.

This entry describes the operations supported by :doc:`the economic
protocol <./protocol>` that implement *enabled* features -- that is,
those available to end-users on Tezos Mainnet. The complete list of
operations, including those corresponding to features in development
or available only on test networks, is given in the
:package-api:`OCaml Documentation
<tezos-protocol-alpha/Tezos_raw_protocol_alpha/Operation_repr/index.html>`.

.. _validation_passes_alpha:

Validation Passes
~~~~~~~~~~~~~~~~~

The different kinds of operations are grouped into classes. Each class
has an associated index, a natural number, also known as a
:ref:`validation pass<shell_header>`. There are currently four classes
of operations: :ref:`consensus <consensus_operations_alpha>`,
:ref:`voting <voting_operations_alpha>`,
:ref:`anonymous<anonymous_operations_alpha>`, and :ref:`manager
operations<manager_operations_alpha>`. This order also specifies the
:ref:`validation and application<operation_validity_alpha>` priority
of each of these classes. Consensus operations are considered the
highest priority ones, and manager operations the lowest.

Each kind of operation belongs to exactly one validation pass, except for the :ref:`failing_noop_alpha` which belongs to no validation pass and therefore cannot be :ref:`applied<operation_validity_alpha>`.

In the sequel, we describe the different classes of operations, and
the different kinds of operations belonging to each class.

.. _consensus_operations_alpha:

Consensus Operations
~~~~~~~~~~~~~~~~~~~~

.. TODO tezos/tezos#4204: document PCQ/PQ

Consensus operations are administrative operations that are necessary
to implement the :doc:`consensus algorithm<consensus>`. There are two
kinds of consensus operations, each belonging to the different voting
phases required to agree on the next block.

- A ``Preattestation`` operation implements a first vote for a
  :ref:`candidate block <candidate_block_alpha>` with the aim of
  building a :ref:`preattestation quorum <quorum_alpha>`.

- An ``Attestation`` operation implements a vote for a candidate block
  for which a preattestation quorum certificate (PQC) has been
  observed.

.. _voting_operations_alpha:

Voting Operations
~~~~~~~~~~~~~~~~~

Voting operations are operations related to the on-chain :doc:`Tezos
Amendment<voting>` process. In this economic protocol, there are two
voting operations:

- The ``Proposal`` operation enables delegates to submit (also known as
  to "inject") protocol amendment proposals, or to up-vote previously
  submitted proposals, during the Proposal period.

- The ``Ballot`` operation enables delegates to participate in the
  Exploration and Promotion periods. Delegates use this operation to
  vote for (``Yea``), against (``Nay``), or to side with the majority
  (``Pass``), when examining a protocol amendment proposal.

Further details on each operation's implementation and semantics are
provided in the dedicated entry for :doc:`on-chain governance<voting>`.

.. _anonymous_operations_alpha:

Anonymous Operations
~~~~~~~~~~~~~~~~~~~~

This class groups all operations that do not require a signature from
a Tezos account (with an exception, detailed below). They implement
different functionalities of the protocol, and their common
characteristic is that they allow the account originating these
operations to remain anonymous in order to avoid censorship.

Two operations in this class implement functionality pertaining to the
protocol's :doc:`random seeds generation
mechanism<randomness_generation>`:

- The ``Seed_nonce_revelation`` operation allows a baker to
  anonymously reveal the nonce seed for the commitment it had included
  in a previously baked block (in the previous cycle).

- The ``Vdf_revelation`` operation allows the submission of a solution
  to, and a proof of correctness of, the :ref:`VDF
  challenge<vdf_alpha>` corresponding to the VDF revelation period of
  the randomness generation protocol.

Further details on the latter operation's implementation and semantics
are provided in the :ref:`random seed generation
protocol<randomness_generation_alpha>`.

Three operations in this class are used to :ref:`punish participants
which engage in Byzantine behaviour<slashing_alpha>` -- notably
delegates which :ref:`"double sign" <def_double_signing_alpha>` blocks, or emit
conflicting :ref:`consensus operations<consensus_operations_alpha>`:

- The ``Double_preattestation_evidence`` operation allows for accusing
  a delegate of having *double-preattested* -- i.e., of having
  preattested two different block candidates, at the same level and at
  the same round. The bulk of the evidence, the two arguments
  provided, consists of the two offending preattestations.

- Similarly, the ``Double_attestation_evidence`` operation allows for
  accusing a delegate of having *double-attested* -- i.e., of having
  attested two different block candidates at the same level and the
  same round -- by providing the two offending attestations.

- The ``Double_baking_evidence`` allows for accusing a delegate of
  having "double-baked" a block -- i.e., of having signed two
  different blocks at the same level and at same round. The bulk of
  the evidence consists of the :ref:`block
  headers<block_contents_alpha>` of each of the two offending blocks.

See :ref:`here<slashing_alpha>` for further detail on the semantics of
evidence-providing operations.

The ``Activation`` operation allows users which participated in the
Tezos fundraiser to make their :ref:`accounts <def_account_alpha>` operational.

Finally, the ``Drain_delegate`` operation allows an active
consensus-key account, i.e., an account to which a baker delegated its
consensus-signing responsibility, to **empty** its delegate
account. This operation is used as a deterrent to ensure that a
delegate secures its consensus key as much as its manager (or main)
key.

.. _manager_operations_alpha:

Manager Operations
~~~~~~~~~~~~~~~~~~

.. FIXME tezos/tezos#3936: integrate consensus keys operations.

.. FIXME tezos/tezos#3937:

   Document increased paid storage manager operation.

Manager operations enable end-users to interact with the Tezos
blockchain -- e.g., transferring funds or calling :doc:`smart
contracts<michelson>`. A manager operation is issued by a single
*manager* account which signs the operation and pays the
:ref:`fees<def_fee_alpha>` to the baker for its inclusion in a block. Indeed,
manager operations are the only fee-paying and
:ref:`gas-consuming<def_gas_alpha>` operations.

- The ``Reveal`` operation reveals the public key of the sending
  manager. Knowing this public key is indeed necessary to check the signature
  of future operations signed by this manager.
- The ``Transaction`` operation allows users to either transfer tez
  between accounts and/or to invoke a smart contract.
- The ``Delegation`` operation allows users to :ref:`delegate their
  stake <delegating_coins>` to a :ref:`delegate<def_delegate_alpha>` (a
  *baker*), or to register themselves as delegates.
- The ``Update_consensus_key`` operation allows users to delegate the
  responsibility of signing blocks and consensus-related operations to
  another account. Note that consensus keys cannot be BLS public keys.
- The ``Origination`` operation is used to
  :ref:`originate<def_origination_alpha>`, that is to deploy, smart contracts
  in the Tezos blockchain.
- The ``Set_deposits_limit`` operation enables delegates to adjust the
  amount of stake a delegate :ref:`has locked in
  bonds<active_stake_alpha>`.
- Support for registering global constants is implemented with the
  ``Register_global_constant`` operation.
- The ``Increase_paid_storage`` operation allows a sender to increase
  the paid storage of some previously deployed contract.
- The ``Event`` operation enables sending event-like information to
  external applications from Tezos smart contracts -- see
  :doc:`Contract Events<event>` for further detail.

Moreover, all operations necessary to implement Tezos' *enshrined*
Layer 2 solutions into the economic protocol are also manager
operations.

In particular, :doc:`smart rollups <smart_rollups>` maintenance is
handled with dedicated manager operations.

- The ``Smart_rollup_originate`` operation is used to originate, that
  is, to deploy smart rollups in the Tezos blockchain.
- The ``Smart_rollup_add_messages`` operation is used to add messages
  to the inbox shared by all the smart rollups originated in the Tezos
  blockchain. These messages are interpreted by the smart rollups
  according to their specific semantics.
- The ``Smart_rollup_publish`` operation is used to regularly declare
  what is the new state of a given smart rollup in a so-called
  “commitment”. To publish commitments, an implicit account has to
  own at least ꜩ 10,000, which are frozen as long as at least one of
  their commitments is disputable.
- The ``Smart_rollup_cement`` operation is used to cement a
  commitment, if the following requirements are met: it has been
  published for long enough, and there is no concurrent commitment for
  the same state update. Once a commitment is cemented, it cannot be
  disputed anymore.
- The ``Smart_rollup_recover_bond`` operation is used by an implicit
  account to unfreeze their ꜩ 10,000. This operation only succeeds if
  and only if all the commitments published by the implicit account
  have been cemented.
- The ``Smart_rollup_refute`` operation is used to start or pursue a
  dispute. A dispute is resolved on the Tezos blockchain through a
  so-called refutation game, where two players seek to prove the
  correctness of their respective commitment. The game consists in a
  dissection phase, where the two players narrow down their
  disagreement to a single execution step, and a resolution, where the
  players provide a proof sustaining their claims. The looser of a
  dispute looses their frozen bond: half of it is burned, and the
  winner receives the other half in compensation.
- The ``Smart_rollup_timeout`` operation is used to put an end to a
  dispute if one of the two players takes too much time to send their
  next move (with a ``Smart_rollup_refute`` operation). It is not
  necessary to be one of the players to send this operation.
- The ``Smart_rollup_execute_outbox_message`` operation is used to
  enact a transaction from a smart rollup to a smart contract, as
  authorized by a cemented commitment. The targeted smart contract can
  determine if it is called by a smart rollup using the ``SENDER``
  Michelson instruction.

.. _manager_operations_batches_alpha:

Manager Operation Batches
"""""""""""""""""""""""""

Manager operations can be grouped, forming a so-called
**batch**. Batches enable the inclusion of several manager operations
from the same manager in a single block.

Batches satisfy the following properties:

- All operations in a batch are issued by the same manager, which
  provides a single signature for the entire batch.
- A batch is :ref:`applied<manager_operations_application_alpha>`
  atomically: all its operations are executed sequentially, without
  interleaving other operations. Either all the operations in the
  batch succeed, or none is applied.

.. _failing_noop_alpha:

Failing_noop operation
~~~~~~~~~~~~~~~~~~~~~~

The ``Failing_noop`` operation is not executable in the protocol:

- it can only be validated in :ref:`mempool mode <partial_construction_alpha>`, by the :doc:`prevalidator component <../shell/prevalidation>`;
- consequently, this operation cannot be :ref:`applied <operation_validity_alpha>`, and in fact will never be included into a block.

Rather, the ``Failing_noop`` operation allows
to sign an arbitrary string, without introducing an operation that could be misinterpreted in the protocol.

The Octez client provides commands to sign and verify the signature of input messages by a given key. These commands create a ``failing_noop``
operation from the message that is being signed or checked.

::

   octez-client sign message "hello world" for <account>

   octez-client check that message "hello world" was signed by <account> to
   produce <signature>
