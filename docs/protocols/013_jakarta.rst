Protocol Jakarta
================

**Important**: revision ``PtJakarta...nGw`` of protocol Jakarta contains
`two critical bugs <https://research-development.nomadic-labs.com/we-found-two-bugs-in-torus-jakarta.html>`_
that have been corrected in the latest version ``PtJakart2...SqY``.

This page contains all the relevant information for protocol Jakarta
(see :ref:`naming_convention`).

The code can be found in the :src:`src/proto_013_PtJakart` directory of the
``master`` branch of Tezos.

This page documents the changes brought by protocol Jakarta with respect
to Ithaca.

.. contents::

New Environment Version (V5)
----------------------------

This protocol requires a different protocol environment than Ithaca.
It requires protocol environment V5, compared to V4 for Ithaca.
(MR :gl:`!4071`)

- Remove compatibility layers. (MR :gl:`!4215`)

- Lwtreslib updates from stdlib 4.13. (MR :gl:`!4279`)

- Update to data encoding 0.5 and support compact encoding. (MRs :gl:`!4339`,
  :gl:`!4582`)

- Add Merkle proofs to the protocol. (MRs :gl:`!4086`, :gl:`!4307`, :gl:`!4509`,
  :gl:`!4536`, :gl:`!4694`)

- Update Bls_signature to bls12-381.3.0.1. (MRs :gl:`!4383`, :gl:`!4732`)

- Add pk/signature_size_in_bytes in Bls_signature. (MR :gl:`!4492`)

- Add size_in_memory for BLS types and reset test configurations of for the
  typed IR size to previous values. (MR :gl:`!4464`)

- Provide let* binding operators. (MR :gl:`!4365`)

- Expose ``Blake2b.Make_merkle_tree``. (MR :gl:`!4618`)

- Sync interfaces with upstreams. (MR :gl:`!4617`)

- Export context configuration. (MR :gl:`!4601`)

- Remove unused function register_resolver. (MR :gl:`!4591`)

Liquidity Baking
----------------

Several changes are made to the Liquidity Baking Escape Vote (MR :gl:`!4201`):

- The options are renamed ``On`` (instead of ``false``) and ``Off``
  (instead of ``true``) to reduce confusion.

- A third ``Pass`` option is added. When this option is used the
  exponential moving average (EMA) of escape votes is not affected by
  the block. Note to developers of baking software: we don't recommend to
  use this option as a default value; instead we recommend to force the user
  to explicitly choose one of the three options; this behavior has been
  implemented in Octez' ``tezos-baker``.

- The escape hatch threshold is reset to 50% to account for the new
  symmetry in the escape vote introduced by ``Pass`` option.

- The precision of the EMA computation has been increased by a factor
  of 1000. To achieve this without overflowing, this computation is
  now performed using arbitrary-precision arithmetic. The EMA itself
  and the EMA threshold are still stored on 32bits.

- EMA is always rounded toward the threshold.

- When the EMA reaches the threshold, the deactivation of the subsidy
  is not permanent anymore. If the proportion of bakers voting ``On``
  later increases and the EMA falls back below the threshold then the
  subsidy is restarted.

- The Liquidity Baking Escape Vote is renamed into "Liquidity Baking
  Toggle Vote".

Transaction Optimistic Rollups
------------------------------

Rollups is a new feature of the protocol supporting execution of transactions
off-chain. (MRs :gl:`!3915`, :gl:`!4198`, :gl:`!4200`, :gl:`!4203`, :gl:`!4332`,
:gl:`!4428`, :gl:`!4309`, :gl:`!4360`, :gl:`!4369`, :gl:`!4447`, :gl:`!4357`,
:gl:`!4344`, :gl:`!4480`, :gl:`!4275`, :gl:`!4017`, :gl:`!4489`, :gl:`!4499`,
:gl:`!4496`, :gl:`!4515`, :gl:`!4453`, :gl:`!4508`, :gl:`!4531`, :gl:`!4484`,
:gl:`!4495`, :gl:`!4446`, :gl:`!4561`, :gl:`!4538`, :gl:`!4593`, :gl:`!4583`,
:gl:`!4548`, :gl:`!4594`, :gl:`!4590`, :gl:`!4603`, :gl:`!4604`, :gl:`!4576`,
:gl:`!4517`, :gl:`!4572`, :gl:`!4634`, :gl:`!4653`, :gl:`!4521`, :gl:`!4649`,
:gl:`!4668`, :gl:`!4523`, :gl:`!4664`, :gl:`!4635`, :gl:`!4667`, :gl:`!4758`,
:gl:`!4703`, :gl:`!4739`, :gl:`!4702`, :gl:`!4755`, :gl:`!4756`, :gl:`!4768`,
:gl:`!4750`, :gl:`!4733`, :gl:`!4726`, :gl:`!4780`, :gl:`!4779`, :gl:`!4784`,
:gl:`!4772`, :gl:`!4782`, :gl:`!4790`, :gl:`!4801`, :gl:`!4740`, :gl:`!4376`,
:gl:`!4793`, :gl:`!4841`, :gl:`!4930`, :gl:`!4917`, :gl:`!4835`, :gl:`!4951`,
:gl:`!4878`, :gl:`!4913`, :gl:`!4956`, :gl:`!4963`, :gl:`!4965`, :gl:`!4972`,
:gl:`!4976`, :gl:`!4984`, :gl:`!4999`, :gl:`!5078`, :gl:`!5080`)

Smart Contract Optimistic Rollups (ongoing)
-------------------------------------------

Support for execution of smart contracts inside rollups is in progress and is
not enabled yet. (MRs :gl:`!3941`, :gl:`!4000`, :gl:`!4020`, :gl:`!4148`,
:gl:`!4173`, :gl:`!4483`, :gl:`!4563`, :gl:`!4629`, :gl:`!4586`, :gl:`!4621`,
:gl:`!4700`, :gl:`!4343`, :gl:`!4770`, :gl:`!4791`, :gl:`!4498`, :gl:`!4620`,
:gl:`!4568`, :gl:`!4289`)

Tickets Hardening
-----------------

Ticket handling has been improved via runtime token validation layer, which adds
extra layer of protection for ticket semantics. (MRs :gl:`!4011`, :gl:`!3826`,
:gl:`!4303`, :gl:`!4168`, :gl:`!4323`, :gl:`!4334`, :gl:`!4426`, :gl:`!4491`,
:gl:`!4341`, :gl:`!4190`)

Voting procedure
----------------

The voting power of a delegate is no longer rounded to rolls, it is
now instead the full staking power of the delegate, currently
expressed in mutez. (MR :gl:`!4265`)

Breaking Changes
----------------

- The binary encoding of the result of the ``Transaction`` operation
  has changed.  Its contents now vary depending on the kind of
  destination. The default cases (implicit and smart contracts) are
  prefixed with the tag ``0``. (MR :gl:`!4205`)

- The ``consumed_gas`` field in the encoding of operations becomes
  **deprecated** in favour of ``consumed_milligas``, which contains
  a more precise readout for the same value. ``consumed_milligas``
  field was added to the encoding of block metadata for uniformity.
  (MR :gl:`!4388`)

- The following RPCs output format changed:

  1. ``/chains/<chain_id>/blocks/<block>/votes/proposals``,
  2. ``/chains/<chain_id>/blocks/<block>/votes/ballots``,
  3. ``/chains/<chain_id>/blocks/<block>/votes/listings``,
  4. ``/chains/<chain_id>/blocks/<block>/votes/total_voting_power``,
  5. ``/chains/<chain_id>/blocks/<block>/context/delegates/<public_key_hash>``
  6. ``/chains/<chain_id>/blocks/<block>/context/delegates/<public_key_hash>/voting_power``

  The voting power that was represented by ``int32`` (denoting rolls)
  is now represented by an ``int64`` (denoting mutez). Furthermore, in
  the RPC ``/chains/<chain_id>/blocks/<block>/votes/listings``, the
  field ``rolls`` has been replaced by the field ``voting_power``. (MR :gl:`!4265`)

- Encoding of transaction and origination operations no longer contains
  deprecated ``big_map_diff`` field. ``lazy_storage_diff`` should be used
  instead. (MR: :gl:`!4387`)

- The JSON and binary encodings for Liquidity Baking Toggle Votes have
  changed as follows:

.. list-table:: Changes to encodings of Liquidity Baking Toggle Vote
   :widths: 20 20 20 20 20
   :header-rows: 1

   * - Vote option
     - Old binary encoding
     - Old JSON encoding
     - New binary encoding
     - New JSON encoding

   * - ``On``
     - ``0x00``
     - ``false``
     - ``0x00``
     - ``"on"``

   * - ``Off``
     - any other byte
     - ``true``
     - ``0x01``
     - ``"off"``

   * - ``Pass``
     - N/A
     - N/A
     - ``0x02``
     - ``"pass"``

- The values of the Liquidity Baking EMA in block receipts and the
  Liquidity Baking EMA threshold in the constants have been scaled by
  1000, the new value of the threshold is 1,000,000,000. To compute
  the proportion Off/(On + Off) of toggle votes the following formula
  can be used: liquidity_baking_toggle_ema / 2,000,000,000.

- Receipts and balance updates may now refer to a new type of account
  with the kind ``"freezer"`` and the category ``"bonds"``.
  Bonds are like deposits.
  However, they can be associated to implicit or originated accounts,
  unlike deposits that only apply to implicit accounts that are also
  delegates (see :doc:`../active/token_management`).
  (MR :gl:`!4437`)

Bug Fixes
---------

- Expose ``consumed_milligas`` in the receipt of the ``Register_global_constant``
  operation. (MR :gl:`!3981`)

- Refuse operations with inconsistent counters. (MR :gl:`!4024`)

- Normalize scripts in optimized format during origination. (MR :gl:`!3852`)

Minor Changes
-------------

- The RPC ``../context/delegates`` can take two additional Boolean flags, ``with_minimal_stake`` or ``without_minimal_stake``, to select delegates that have at least a minimal stake to participate in consensus and governance, or do not have such a minimal stake, respectively. (MR :gl:`!3951`)

- Make cache layout a parametric constant of the protocol. (MR :gl:`!4035`)

- Change ``blocks_per_voting period`` in context with ``cycles_per_voting_period`` (MR :gl:`!4456`)

- Use dedicated error for duplicate ballots. (MR :gl:`!4209`)

- Allow committee size to be < 4. (MR :gl:`!4308`)

- Remove delegate_selection parameter and introduce initial_seed parameter.
  (MR :gl:`!3842`)

- Retrieve a contract's public key before prechecking an operation. (MR :gl:`!4877`)

- Normalize types and code of views in RPCs and client commands.
  (MR :gl:`!4911`)

Michelson
---------

- Some operations are now forbidden in views: ``CREATE_CONTRACT``,
  ``SET_DELEGATE`` and ``TRANSFER_TOKENS`` cannot be used at the top-level of a
  view because they are stateful, and ``SELF`` because the entry-point does not
  make sense in a view.
  However, ``CREATE_CONTRACT``, ``SET_DELEGATE`` and ``TRANSFER_TOKENS`` remain
  available in lambdas defined inside a view.
  (MR :gl:`!3737`)

- Non-entrypoint annotations are ignored by the typechecker and not propagated.

  - All contracts that used to typecheck correctly before will still typecheck
    correctly afterwards. Though more contracts are accepted as branches with
    different annotations won't be rejected anymore.

  - The special annotation ``%@`` of ``PAIR`` has no effect.

  - RPCs
    ``/chains/<chain_id>/blocks/<block>/context/contracts/<address>/typecheck_code``,
    ``/chains/<chain_id>/blocks/<block>/context/contracts/<address>/trace_code``,
    as well as typechecking errors reporting stack types, won't report
    annotations anymore.

    In their output encodings, the objects containing the fields ``item`` and
    ``annot`` are replaced with the contents of the field ``item``.

  - RPCs ``/chains/<chain_id>/blocks/<block>/context/contracts/<address>``,
    ``/chains/<chain_id>/blocks/<block>/context/contracts/<address>/script/normalized``,
    ``/chains/<chain_id>/blocks/<block>/context/contracts/<address>/entrypoints``,
    ``/chains/<chain_id>/blocks/<block>/context/contracts/<address>/entrypoints/normalized``,
    ``/chains/<chain_id>/blocks/<block>/context/contracts/<address>/entrypoints/<entrypoint>``,
    ``/chains/<chain_id>/blocks/<block>/context/contracts/<address>/entrypoints/<entrypoint>/normalized``
    accept a new boolean parameter ``normalize_types`` to show types without
    their annotations.

  - (MRs :gl:`!4139`, :gl:`!4140`, :gl:`!4141`, :gl:`!4175`, :gl:`!4311`,
    :gl:`!4259`, :gl:`!4844`, :gl:`!4876`, :gl:`!4893`)

- Annotating the parameter toplevel constructor to designate the root entrypoint
  is now forbidden. Put the annotation on the parameter type instead.
  E.g. replace ``parameter %a int;`` by ``parameter (int %a);``
  (MR :gl:`!4366`)

- The ``VOTING_POWER`` of a contract is no longer rounded to rolls. It
  is now instead the full staking power of the delegate, currently
  expressed in mutez. Though, developers should not rely on
  ``VOTING_POWER`` to query the staking power of a contract in
  ``mutez``: the value returned by ``VOTING_POWER`` is still of type`
  ``nat`` and it should only be considered relative to
  ``TOTAL_VOTING_POWER``. (MR :gl:`!4265`)

- The new type ``tx_rollup_l2_address`` has been introduced. It is
  used to identify accounts on transaction rollups’ legders. Values of
  type ``tx_rollup_l2_address`` are 20-byte hashes of a BLS
  public keys (with a string notation based of a base58 encoding,
  prefixed with ``tz4``). (MR :gl:`!4431`)

- A new instruction ``MIN_BLOCK_TIME`` has been added. It can be used to
  push the current minimal time between blocks onto the stack. The value is
  obtained from the protocol's ``minimal_block_delay`` constant.
  (MR :gl:`!4471`)

- The existing type ``sapling_transaction`` is renamed
  ``sapling_transaction_deprecated`` and is deprecated. Existing onchain contracts
  are automatically converted.
  A new Michelson type ``sapling_transaction`` and an overload of the
  instruction ``SAPLING_VERIFY_UPDATE`` have been added to fix the malleability
  problem of the old instruction
  (see :doc:`Sapling integration<../alpha/sapling>`).
  (MRs :gl:`!4670`, :gl:`!4589`)

- The protocol migration to Alpha will ensure that 8 smart contracts
  on mainnet that depend on features deprecated in Babylon, type check
  under the new protocol without the ``--legacy`` flag.  This will be
  ensured by patching the contracts. Their behaviour will remain
  unchanged which was tested by replaying historic transactions to
  these smart contracts with the updated code. For more information on
  which contracts are patched and how the change can be tested read
  the description of the MR that brought the migration. (MRs
  :gl:`!3730`, :gl:`!4681`)

RPC Changes
-----------

- Add ``selected_snapshot`` RPC that replaces deleted ``roll_snapshot``.
  (MRs :gl:`!4479`, :gl:`!4585`)

Internal
--------

The following changes are not visible to the users but reflect
improvements of the codebase.

- ``BALANCE`` is now passed to the Michelson interpreter as a step constant
  instead of being read from the context each time this instruction is
  executed. (MR :gl:`!3871`)

- Faster gas monad. (MR :gl:`!4034`)

- Carbonated map utility module. (MRs :gl:`!3845`, :gl:`!4156`)

- Rewrite step constants explicitly when entering a view. (MR :gl:`!4230`)

- Update migration for Ithaca. (MR :gl:`!4107`)

- Tenderbake: Optimizing round_and_offset. (MR :gl:`!4009`)

- Make protocol easier to translate to Coq. (MR :gl:`!4260`)

- Generalize the destination argument of Transaction. (MR :gl:`!4205`)

- Do not propagate operations conditioned by a feature flag. (MR :gl:`!4330`)

- Optimize local gas counter exhaustion checking. (MR :gl:`!4305`)

- Fix edge case in pseudorandom computations. (MR :gl:`!4385`)

- Ensure voting periods end at cycle ends. (MR :gl:`!4425`)

- Gas: move Size module to lib_protocol. (MR :gl:`!4337`)

- Cleanup Tenderbake code. (MRs :gl:`!4423`, :gl:`!4436`, :gl:`!4225`,)

- Fix coq:lint error ignoring message (MR :gl:`!4473`)

- Take user/automatic protocol upgrades into account during operation
  simulation. (MR :gl:`!4433`)

- Improve gas model of unparse_script. (MR :gl:`!4328`)

- Remove unreachable code (MR :gl:`!4615`)

- Separate internal operations. (MRs :gl:`!4613`, :gl:`!4623`, :gl:`!4632`,
  :gl:`!4643`)

- Stakable frozen bonds. (MR :gl:`!4437`)

- Rename first_level_of_Tenderbake for consistency. (MR :gl:`!4825`)

- Update gas cost model. (MR :gl:`!4840`)

- Update size cost model. (MR :gl:`!4948`)

- Internal refactorings in Michelson typechecker and interpreter. (MRs
  :gl:`!4502`, :gl:`!4693`, :gl:`!4692`, :gl:`!4658`, :gl:`!4507`, :gl:`!4578`,
  :gl:`!4506`, :gl:`!4133`, :gl:`!4429`, :gl:`!4427`, :gl:`!4298`, :gl:`!4297`,
  :gl:`!4363`, :gl:`!4364`, :gl:`!3863`, :gl:`!3696`)

- Other internal refactorings or documentation. (MRs :gl:`!4276`,
  :gl:`!4457`, :gl:`!3928`, :gl:`!4041`, :gl:`!4088`, :gl:`!3755`,
  :gl:`!4128`, :gl:`!4262`, :gl:`!4257`, :gl:`!4293`, :gl:`!4444`,
  :gl:`!4224`, :gl:`!4432`, :gl:`!4468`, :gl:`!4777`, :gl:`!4820`, :gl:`!4287`)
