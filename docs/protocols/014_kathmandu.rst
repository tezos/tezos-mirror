Protocol Kathmandu
==================

This page contains all the relevant information for protocol Kathmandu
(see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_014_PtKathma` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Kathmandu with respect
to Jakarta.

.. contents::

New Environment Version (V6)
----------------------------

This protocol requires a different protocol environment than Jakarta.
It requires protocol environment V6, compared to V5 for Jakarta.
(MR :gl:`!4961`)

- Upgrade to OCaml 4.14. (MR :gl:`!4973`)

- Remove unused modules from environment. (MR :gl:`!5177`)

- Pass ``chain_id`` to ``Protocol.init``. (MR :gl:`!5284`)

- WebAssembly integration. (MRs :gl:`!5529`, :gl:`!5482`, :gl:`!5563`)

- Expose Plonk package. (MR :gl:`!5596`)

- Add VDF library. (MR :gl:`!5064`)

Smart Contract Optimistic Rollups (ongoing)
-------------------------------------------

Rollups supporting execution of smart contracts. (MRs :gl:`!4933`, :gl:`!4812`,
:gl:`!4938`, :gl:`!4765`, :gl:`!4699`, :gl:`!4964`, :gl:`!5138`, :gl:`!5070`,
:gl:`!5219`, :gl:`!5160`, :gl:`!5218`, :gl:`!5125`, :gl:`!4968`, :gl:`!5305`,
:gl:`!5359`, :gl:`!5280`, :gl:`!5202`, :gl:`!5260`, :gl:`!5201`, :gl:`!5349`,
:gl:`!5116`, :gl:`!5383`, :gl:`!5360`, :gl:`!5342`, :gl:`!5351`, :gl:`!5369`,
:gl:`!5412`, :gl:`!5430`, :gl:`!5379`, :gl:`!5435`, :gl:`!5398`, :gl:`!5362`,
:gl:`!5352`, :gl:`!5439`, :gl:`!5396`, :gl:`!5420`, :gl:`!5327`, :gl:`!5416`,
:gl:`!5434`, :gl:`!5442`, :gl:`!5395`, :gl:`!5498`, :gl:`!5492`, :gl:`!5465`,
:gl:`!5505`, :gl:`!5504`, :gl:`!5443`, :gl:`!5543`, :gl:`!5548`, :gl:`!5399`,
:gl:`!5478`, :gl:`!5551`, :gl:`!5493`, :gl:`!5530`, :gl:`!5559`, :gl:`!5519`,
:gl:`!5537`, :gl:`!5555`, :gl:`!5544`, :gl:`!5521`, :gl:`!5568`, :gl:`!5609`,
:gl:`!5516`, :gl:`!5549`)

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain.  (MRs :gl:`!5371`,
:gl:`!5501`, :gl:`!5508`, :gl:`!5527`, :gl:`!5423`)

Updated randomness generation
-----------------------------

Introduce a new randomness generation protocol based on Verifiable Delay
Functions (VDFs). See :doc:`Randomness generation <../active/randomness_generation>`
for an explanation of how this protocol works.
(MRs :gl:`!5064`, :gl:`!5848`)

Contract Event Logging
----------------------

Contracts may now emit events thanks to a new ``EMIT`` instruction.

See :doc:`Event <../alpha/event>` for more information.
(MRs :gl:`!4656`, :gl:`!5715`, :gl:`!5724`, :gl:`!5731`)

Increase_paid_storage
---------------------

- Increase_paid_storage is a new operation that enable a payer to increase the
  paid storage of a smart contract by some bytes amount. (MR :gl:`!5605`)

Breaking Changes
----------------

- Reveal operations can only occur at the head of a manager operation
  batch (MR :gl:`!5182`).

- Restore *all-or-nothing* semantics of manager operation batches by
  enforcing that failing reveal operations do not take effect (MR
  :gl:`!5182`).

- Operations with non-deserializable scripts may now be propagated and
  included in blocks. If such an operation is in a block, its
  application will fail so the operation will have no effect, but its
  fees will still be taken. (MR :gl:`!5506`)

- The one-operation-per-manager-per-block restriction (1M) is now
  enforced in blocks. It was previously (optionally) enforced by the
  prevalidator using the plugin mempool filters. This meant that a
  baker could still include several operations from the same manager
  in its own block. This is no longer possible: the application of a
  block containing more than one operation from the same manager will
  now fail. (MR :gl:`!5557`)

- VDF phase added after RANDAO in randomness generation. Nonces for generating
  the random seed must now be revealed in the first 256 blocks of a cycle
  instead of anytime in a cycle. (MRs :gl:`!5064`, :gl:`!5848`)

RPC Changes
-----------

- Add a new RPC for querying data found on the voting listings for a
  delegate, i.e. voting power, casted ballots and proposals in the
  current voting period.  (MR :gl:`!4577`)

  ``/chains/<chain_id>/blocks/<block>/context/delegates/<delegate_pkh>/voting_info``

- Add a new RPC to execute contracts' views offchain. (MR :gl:`!4810`)

  ``/chains/<chain_id>/blocks/<block>/helpers/scripts/run_script_view``

- Deprecate the ``endorsing_rights`` RPC for whole cycles, by deprecating the
  ``cycle`` parameter. (MR :gl:`!5082`)

- Some contract RPCs working on originated contracts only may return a different
  error than before on implicit accounts. (MR :gl:`!5373`)

Operation receipts
------------------

- Remove field ``consumed_gas``, deprecated in Jakarta. Use field
  ``consumed_milligas`` instead. (MRs :gl:`!5536`, :gl:`!5703`)

- Operations that are both manager operations and internal operations returned
  by Michelson scripts now have different names for receipt encodings. This
  concerns transations, originations and delegations, where the word "internal"
  explicitly appears in the case of internal operation receipts. (:gl:`!5149`)

- Successful contract execution attaches to the transaction receipt a list of
  contract events. See :doc:`Event <../alpha/event>` for more information.

- New operation ``Vdf_revelation`` introduced for VDF revelation. See
  :doc:`Randomness generation <../active/randomness_generation>` for more
  details.

- New operation ``Increase_paid_storage`` introduced to increase the paid
  storage of a smart contract. (MR :gl:`!5605`)

Bug Fixes
---------

- Consume constant gas ``Michelson_v1_gas.Cost_of.manager_operation``
  during precheck: this fixes some cases of operations passing
  precheck even though they obviously do not have enough gas to apply
  the external operation, e.g. when ``gas_limit = 0``. (MR :gl:`!5506`)

- Emptying an implicit account does not cost extra-gas anymore. (MR
  :gl:`!5566`)

- The ``helpers/scripts/run_operation`` RPC now checks whether all
  operations in a batch have the same source. (MR :gl:`!5557`)

- Fix a discrepancy in gas consumption of contract origination between
  dry run and actual application (MR :gl:`!5659`)

- Fix the ``delegated_balance`` rpc, which reported an incorrect value for
  delegates that have frozen bonds (MR :gl:`!5765`)

Minor Changes
-------------

- New error ``Remove_commitment_too_early`` in TORU. (MR :gl:`!4895`)

- New error ``Inconsistent_sources``. (MR :gl:`!5475`)

- Fix name of encoding of round_overflow. (MR :gl:`!5089`)

- Split internal transactions. (MR :gl:`!5195`)

- Allow implicit accounts to delegate at bootstrap time. (MR :gl:`!5071`)

- Fail when attempting to delegate from unrevealed key at bootstrap.
  (MR :gl:`!5645`)

- Allow to register a governance dictator for testnets and private chains.
  (MRs :gl:`!4547`, :gl:`!5662`, :gl:`!5612`, :gl:`!5751`)

- Update gas for K. (MR :gl:`!5702`)

Internal
--------

- Update migration for Jakarta. (MR :gl:`!5059`)

- Uncurry some functions in Michelson interpreter. (MR :gl:`!5406`)

- Make carbonated maps available to the Raw context (MRs :gl:`!4815`, :gl:`!4891`)

- Move Michelson representation modules above the Alpha_context abstraction
  barrier. (MR :gl:`!4418`)

- Further cleanup on Tenderbake code. (MR :gl:`!4513`)

- Rename ``run_view`` into ``run_tzip4_view`` for consistency with
  ``run_script_view``. Does not affect the existing ``run_view`` RPC.
  (MR :gl:`!4810`)

- Precheck no longer returns the gas it has consumed. Instead of
  "replaying" the gas from precheck, ``apply_manager_contents`` consumes
  the same gas again step by step. (MR :gl:`!5506`)

- Precheck no longer tries to deserialize scripts. It does still check
  that the operation has enough gas for these deserializations (by
  consuming an estimated gas cost based on the bytes size: this has
  not changed). (MR :gl:`!5506`)

- Split precheck into two parts: checks and effects. The checks part
  is effect-free. The effects part consists of the modifications of
  the context that happen regardless of whether the application of the
  operation succeeds: take the fees, increment the account's counter,
  and remove the operation's gas limit from the available block
  gas. The checks part must ensure that the effects part cannot
  fail. (MR :gl:`!5557`)

- Move the checks part of precheck (see above) to a new file
  ``validate_operation.ml``. The effects part remains in
  ``apply_operation`` and is renamed to ``take_fees``. The new
  ``Validate_operation.validate_operation`` function is called before
  ``Apply.apply_operation`` in ``Main``. It stores its own state in
  ``Main.validation_state`` and works with the context from the
  beginning of the block (which is fine thanks to the 1M restriction).
  For now, ``validate_operation`` does nothing for non-manager
  operations, but we plan to extend it to all operations in the
  future. (MR :gl:`!5557`)

- Remove ``Main.check_manager_signature``. Instead,
  ``Main.precheck_manager`` now takes an additional argument that
  indicates whether it should check the signature. (MR :gl:`!5557`)

- Add a type ``Validate_operation.stamp`` in order to guarantee that
  an operation is always validated before it is applied. Indeed, a
  value of this type may only be created in ``Validate_operation``,
  and is required by ``Apply.apply_operation`` and a few other
  functions in ``Apply``. (MR :gl:`!5557`)
- Extract and split parametric constants. (MRs :gl:`!4798`, :gl:`!5246`)

- Expose length function in raw-context. (MR :gl:`!4361`)

- Optimize parsing of destination. (MR :gl:`!5317`)

- Contract-compatible encodings for implicit and originated accounts.
  (MR :gl:`!5222`)

- CPMM address is a smart-contract. (MR :gl:`!5224`)

- Improve gas cost of transfer to implicit account. (MR :gl:`!5261`)

- Restrict external transfers to non-tx-rollups. (MR :gl:`!5326`)

- Remove stack types from kinfo. (MRs :gl:`!4731`, :gl:`!5664`, :gl:`!5676`)

- Fix the size of Micheline code predicted by the size model. (MR :gl:`!5709`)

- Internal refactorings in Michelson typechecker and interpreter. (MRs
  :gl:`!4722`, :gl:`!4723`, :gl:`!5077`, :gl:`!5104`, :gl:`!5474`)

- Other internal refactorings or documentation. (MRs :gl:`!4890`, :gl:`!4721`,
  :gl:`!5113`, :gl:`!5114`, :gl:`!5005`, :gl:`!5188`, :gl:`!5309`, :gl:`!5310`,
  :gl:`!5308`, :gl:`!5312`, :gl:`!5313`, :gl:`!5298`, :gl:`!5374`, :gl:`!5381`,
  :gl:`!5384`, :gl:`!5513`, :gl:`!5494`, :gl:`!5582`, :gl:`!5553`)

Invoice
-------

@g.b.fefe rewarded 3000 ꜩ for code contributions (testnet dictator key) included in this protocol.
(MR :gl:`!5838`)
