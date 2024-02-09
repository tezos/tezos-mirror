Overview of the economic protocol
=================================

Tezos overview
~~~~~~~~~~~~~~

Tezos is a distributed system in which nodes agree upon a chain of blocks of
operations. Tezos is also an account-based crypto-ledger, where an account is
associated to a public-private key pair, and has a balance, that is, a number of
tokens. Tezos is a :doc:`proof-of-stake<proof_of_stake>` system in which any
account that has a minimal stake amount has the right to produce blocks, in
proportion to their balance.

A Tezos node has mainly three roles: it validates blocks and operations, it
broadcasts them to (and retrieves them from) other nodes, and it maintains a
main chain and its associated state (i.e. the ledger), which includes accounts
and their balances, among other things. Note that, as blocks only specify a
predecessor block, exchanged blocks do not necessarily form a chain, but rather
a tree. Nodes communicate over :doc:`a gossip network<../shell/p2p>`.

A Tezos node acts as a server, which responds to queries and requests from
clients. Such queries and requests are implemented via :doc:`RPC
calls<../developer/rpc>`. A client can query the chain’s state and can inject
blocks and operations into a node. One particular client is the :ref:`baker daemon <baker_run>`,
which is associated to an account. In particular the baker has access to the
account’s private key and thus can sign blocks and operations.

The main reason for using such a client-server architecture is safety: to insulate
the component that has access to the client keys, i.e. the baker, from the
component which is exposed to the internet, i.e. the node. Indeed, the node and
the baker can sit on different computers and the baker does not need to be
exposed to the internet. So nodes manage communication and shield bakers from
network attacks, and bakers hold secrets and bake blocks into the blockchain.

Another advantage of this architecture is that bakers can more easily have
different implementations, and this is important, for instance because different bakers may want
to implement different transaction selection strategies.

Tezos is a self-amending blockchain, in that a large part of Tezos can be
changed through a so-called amendment procedure. To this end, as mentioned in
:ref:`the big picture<the_big_picture>`, a Tezos node consists of two
components:

- the shell, which comprises the network and storage layer, and embeds
- the economic protocol component, which is the part that can be changed through amendment.

The role of the economic protocol
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. FIXME tezos/tezos#3921:

   Update for pipelined validation up to Lima.

At a very high level, a protocol must:

- implement protocol-specific types, such as the type of operations or
  protocol-specific block header data (in addition to the shell
  generic header),

- define under which conditions a block is a valid extension of the
  current blockchain, and define an ordering on blocks to arbitrate
  between concurrent extensions.

Validity conditions are implemented in the ``apply`` function which is
called whenever the node processes a block---see the dedicated
:doc:`protocol validation and operation<validation>` entry for further
detail into the validation and application process for
:ref:`blocks<block_validation_overview_oxford>` and their
:ref:`operations<operation_validity_oxford>`.

.. _shell_proto_interact:
.. _shell_proto_interact_oxford:

Shell-protocol interaction
~~~~~~~~~~~~~~~~~~~~~~~~~~

In the Tezos :ref:`architecture<the_big_picture>`, the economic
protocol and the shell interact in order to ensure that the blocks
being appended to the blockchain are valid. There are mainly two rules
that the shell uses when receiving a new block:

- The shell does not accept a block whose level is below the current
  :ref:`checkpoint<checkpoint>`. The checkpoint itself is updated based on
  information resulting from successful block applications by the
  protocol which depends on the protocol consensus algorithm. Previously
  accepted blocks with lower levels than the current checkpoint are
  considered finalized and immutable.
- The shell changes the head of the chain to this new block only if
  the block is :doc:`valid<../shell/validation>`, and it has a higher
  fitness than the current head; a block is
  :ref:`valid<block_validation_overview_oxford>` only if all the
  operations included are also
  :ref:`valid<operation_validity_oxford>`.

The support provided by the protocol for validating blocks can be
modulated by different :ref:`validation
modes<validation_modes_oxford>`. They allow using this same
interface for quite different use cases, as follows:

- being able to :ref:`apply<full_application_oxford>` a block,
  typically used by the shell's :doc:`validator <../shell/validation>`
  component;
- being able to :ref:`construct<full_construction_oxford>` a block,
  typically used by the baker daemon to *bake* -- that is, to produce
  -- a new block;
- being able to :ref:`partially construct<partial_construction_oxford>`
  a block, typically used by the :doc:`prevalidator
  <../shell/prevalidation>` to determine valid operations in the
  mempool; and,
- being able to :ref:`pre-apply<partial_application_oxford>` a
  block, typically used in the :doc:`validator <../shell/validation>`
  to precheck a block, avoiding to further consider invalid blocks.

.. _block_contents:
.. _block_contents_oxford:

Blocks, Operations and their Validation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. FIXME tezos/tezos#3914:

   Integrate protocol-specific block parts in the blocks and ops
   entry.

A block consists of a header and operations. A block's header is
composed of two parts: :ref:`the protocol-agnostic part<shell_header>`
and :ref:`the protocol-specific part<shell_proto_revisit_oxford>`.
This separation enables the shell to interact with different
protocols. Each Tezos economic protocol can specify different kinds of
operations, which are described further in detail in
:doc:`./blocks_ops`.

The semantics of, respectively, operations and blocks is indeed also
dependent on each economic protocol. The :doc:`Validation and
Application<validation>` entry explains the internals of *validation*
-- that is, how to determine whether operations and blocks can be
safely included in the Tezos blockchain -- and *application* --
that is, how the effects of operations and blocks are taken into
account -- for this economic protocol.

.. _protocol_constants:
.. _protocol_constants_oxford:

Protocol constants
~~~~~~~~~~~~~~~~~~

Protocols are tuned by several *protocol constants*, such as the size
of a nonce, or the number of blocks per cycle. One can distinguish two
kinds of protocol constants:

- *fixed* protocol constants, such as the size of a nonce, are values
  wired in the code of a protocol, and can only be changed by protocol
  amendment (that is, by adopting a new protocol)

- *parametric* protocol constants, such as the number of blocks per
  cycle, are values maintained in a read-only data structure that can
  be instantiated differently, for the same protocol, from one network
  to another (for instance, test networks move faster).

The *list* of protocol constants can be found in the OCaml APIs:

- fixed protocol constants are defined in the module
  :package-api:`Constants_repr
  <tezos-protocol-018-Proxford/Tezos_raw_protocol_018_Proxford/Constants_repr/index.html>`
- parametric constants are defined in the module
  :package-api:`Constants_parametric_repr
  <tezos-protocol-018-Proxford/Tezos_raw_protocol_018_Proxford/Constants_parametric_repr/index.html>`

The *values* of protocol constants in any given protocol can be found using specific RPC calls:

- one RPC for :ref:`all constants <GET_..--block_id--context--constants>`, as shown in :ref:`this example <get_protocol_constants>`
- one RPC for :ref:`the parametric constants <GET_..--block_id--context--constants--parametric>`.

Further documentation of various protocol constants can be found in the subsystems where they conceptually belong.
See, for example:

- :ref:`proof-of-stake parameters <ps_constants_oxford>`.
- :ref:`consensus-related parameters <cs_constants_oxford>`
- :ref:`randomness generation parameters <rg_constants_oxford>`.

See also
~~~~~~~~

An in-depth description of the internals of developing a new Tezos
protocol can be found in the blog post: `How to write a Tezos protocol
<https://research-development.nomadic-labs.com/how-to-write-a-tezos-protocol.html>`_.
