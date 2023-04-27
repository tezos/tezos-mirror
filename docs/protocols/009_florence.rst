Protocol Florence
=================

This page contains all the relevant information for protocol 009 Florence.

The code can be found in the ``src/proto_009_PsFLoren`` directory of the
``master`` branch of Tezos and its full hash is
``PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i``.

This page documents the changes brought by protocol Florence with respect
to Edo.


.. contents:: Summary of changes

Smart Contracts/Michelson
-------------------------

Increase ``max_operation_data_length`` to 32KB
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The maximal size of operations is doubled. In particular this has the
effect of slightly more than doubling the maximum size of a smart
contract.

-  Commit:
   :gl:`tezos@3ff6bc8d`
-  TZIP:
   `increase_operation_size_limit <https://gitlab.com/tezos/tzip/-/blob/bd1a3e6e2169248165df3ba24e24099d7bd1d431/drafts/current/draft-increase_operation_size_limit.md>`__

Fixed a discrepancy between ``CONTRACT`` and ``PACK`` in addresses without entrypoints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Solves issue:
   :gl:`tezos#643`
-  Commit:
   :gl:`tezos@e879b1a7`

Depth-First Execution Order (⚠️ Attention Smart Contract Developers!)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The applied order of inter-contract calls emitted by smart contracts has
changed. The operations are now placed in a stack instead of a queue,
resulting in a depth-first as opposed to breadth-first execution order,
making smart contract development more intuitive.

This change could break contracts that relied on execution order.
The development team has backtested the change against all available
blocks in Delphi, and found only one contract affected by the change, which has
been patched and redeployed. However, smart contract developers should
review their contracts for security threats made possible by the new execution
order.

-  TZIP:
   :gl:`tzip!111`
-  MR:
   :gl:`tezos!2420`


Tooling
-------

Normalization RPCs
~~~~~~~~~~~~~~~~~~

Two new normalization RPCs, ``normalize_data`` and ``normalize_script``,
have been added. They can be used to convert Michelson values and
scripts that have multiple possible representations into a canonical
format. In particular, these RPCs can be used to convert Michelson comb
pairs into the format they had before the introduction of the compact
notations in Edo.

-  Solves issue:
   :gl:`tezos#1016`
-  MR:
   :gl:`tezos!2354`

New ``failing_noop`` Operation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A new operation has been added to the protocol that is guaranteed to
fail. This feature can be used by tooling (such as ``tezos-client``) to
sign arbitrary data securely, without fear of malicious injection into
future protocols.

- Solves issue:
  :gl:`metastatedev/tezos#52`
- MR:
  :gl:`tezos!2361`

Performance
-----------

Endorsements Now Checked in Linear Time (⚠️ Attention Indexers!)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Endorsement and double endorsing evidence operations now contain an
additional slot field; the slot should be the smallest among the
endorser's slots.

Indexers, block explorers, and other software making use of
operations and receipts should be aware that this a breaking
change to the structure of blocks. All other users should be
wholly unaffected.

Most notably, the first list of operations is now composed of
``endorsement_with_slot`` instead of ``endorsement`` operations.

The change will not affect custom signing stacks, as the
``endorsement_with_slot`` is just an unsigned wrapper around the
Edo-compatible ``endorsement`` format.

The reference endorser forges an ``endorsement``, sends it to the
signer, and then wraps the result in an ``endorsement_with_slot``
before injection.

-  Solves issue:
   :gl:`tezos#1028`
-  MR:
   :gl:`tezos!2471`

Staking balance RPC
~~~~~~~~~~~~~~~~~~~

Some users observed degraded performance in v8.1 as reported in issue
:gl:`tezos#1067`. To
address this, the measurement of staking balance has been reworked,
improving the performance of the
``/chains/[...]/blocks/[...]/context/delegates/[...]`` RPC endpoint.

-  MR:
   :gl:`tezos!2547`

Gas Optimizations
~~~~~~~~~~~~~~~~~

Various optimizations have been added to the gas accounting subsystem.
Most notably, gas consumption is now computed using `saturated
arithmetic <https://en.wikipedia.org/wiki/Saturation_arithmetic>`__.

-  MR’s:
   :gl:`tezos!2328`,
   :gl:`tezos!2327`,
   and
   :gl:`tezos!2329`

Governance
----------

Deactivation of the Test Chain in the Economic Protocol
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prior to Florence, Tezos nodes spawned a test chain during the “Testing”
phase of voting for the purpose of allowing users to test the new
amendment. However, this feature was both unused in practice and quite
complex. It has been removed, simplifying the amendment protocol.

Furthermore, the voting periods have been renamed as follows:
  1. Proposal       --> Proposal
  2. Testing_vote   --> Exploration
  3. Testing        --> Cooldown
  4. Promotion_vote --> Promotion
  5. Adoption       --> Adoption

-  TZIP:
   :gl:`tzip!141`
-  MR:
   :gl:`tezos!2469`

Migration
---------

Migrations may now Produce Balance Receipts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Protocol migrations can now update the balance of accounts by producing
balance receipts. This was necessary groundwork for `Baking
Accounts <#Baking-accounts>`__ and facilitates `developer
invoicing <https://www.youtube.com/watch?v=VFY76qFq5Gk>`__.

-  Solves issue:
   :gl:`tezos#138`
-  MR:
   :gl:`tezos!2437`

Internal
--------

Refactoring
~~~~~~~~~~~

Abstract protocol types can now be used consistently outside the
protocol.

-  MR:
   :gl:`tezos!2497`

Authors & Invoice
-----------------

This protocol amendment has been developed by Nomadic Labs, Metastate,
DaiLambda, Marigold, Tarides and the following external contributor:
- `Keefer Taylor <https://gitlab.com/keefertaylor>`__, rewarded ꜩ100 for his
contribution on increasing the maximal operation size.
