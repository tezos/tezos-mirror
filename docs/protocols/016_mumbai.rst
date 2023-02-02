Protocol Mumbai
===============

This page documents the changes brought by protocol Mumbai with respect
to Lima (see :ref:`naming_convention`).

The code can be found in directory :src:`src/proto_016_PtMumbai` of the ``master``
branch of Tezos.

.. contents::

New Environment Version (V8)
----------------------------

This protocol requires a different protocol environment version than Lima.
It requires protocol environment V8, compared to V7 for Lima. (MR :gl:`!6439`)

- Update data-encoding to 0.7.1. (MR :gl:`!6854`)

- Provide only modern bytes and string combinators in environment data encoding.
  (MR :gl:`!6919`)

- Update Plonk in Environment V7 and V8. (MR :gl:`!6925`)

- Remove Plonk from Environment V6. (MR :gl:`!6952`)

Michelson opcodes for logical operations on bytes
-------------------------------------------------

Michelson opcodes ``AND``, ``OR``, ``XOR``, ``NOT``, ``LSL`` and ``LSR``
now support ``bytes``. (MR :gl:`!6055`)

- Michelson opcodes ``NAT``, ``INT`` and ``BYTES`` for bytes-int and bytes-nat
  conversions. (MR :gl:`!6681`)

Smart Rollups
-------------

Rollups supporting execution of smart contracts. (MRs :gl:`!6118`, :gl:`!6425`,
:gl:`!6252`, :gl:`!6396`, :gl:`!6364`, :gl:`!6413`, :gl:`!6545`, :gl:`!6543`,
:gl:`!6590`, :gl:`!6623`, :gl:`!6641`, :gl:`!6549`, :gl:`!6672`, :gl:`!6500`,
:gl:`!6718`, :gl:`!6699`, :gl:`!6630`, :gl:`!6794`, :gl:`!6823`, :gl:`!6828`,
:gl:`!6676`, :gl:`!6698`, :gl:`!6507`, :gl:`!6879`, :gl:`!6860`, :gl:`!6716`,
:gl:`!6468`, :gl:`!6857`, :gl:`!6652`, :gl:`!6913`, :gl:`!6838`, :gl:`!6914`,
:gl:`!6717`, :gl:`!6560`, :gl:`!6891`, :gl:`!6726`, :gl:`!6935`, :gl:`!6951`,
:gl:`!6946`, :gl:`!6954`, :gl:`!6942`, :gl:`!6963`, :gl:`!5741`, :gl:`!6974`,
:gl:`!7013`, :gl:`!7015`, :gl:`!6980`, :gl:`!6993`, :gl:`!6949`, :gl:`!6930`,
:gl:`!6991`, :gl:`!6955`, :gl:`!6937`, :gl:`!7010`, :gl:`!7027`, :gl:`!7034`,
:gl:`!7045`, :gl:`!7036`, :gl:`!6987`, :gl:`!7016`, :gl:`!6380`, :gl:`!7109`,
:gl:`!7114`, :gl:`!6968`, :gl:`!7130`, :gl:`!7054`, :gl:`!7189`, :gl:`!7220`,
:gl:`!7067`, :gl:`!7213`, :gl:`!7233`, :gl:`!7247`, :gl:`!7248`, :gl:`!7253`,
:gl:`!7254`, :gl:`!7271`, :gl:`!7161`, :gl:`!7320`, :gl:`!7280`)

Zero Knowledge Rollups (ongoing)
--------------------------------

Rollups supporting cryptographic proofs of correct execution. (MRs :gl:`!6047`,
:gl:`!6884`, :gl:`!6836`, :gl:`!6855`, :gl:`!6839`)

Data Availability Layer (ongoing)
---------------------------------

Distribution of rollup operations data off-chain. (MRs :gl:`!6174`, :gl:`!6388`,
:gl:`!6527`, :gl:`!6505`, :gl:`!6553`, :gl:`!6685`, :gl:`!6470`, :gl:`!6643`,
:gl:`!6704`, :gl:`!6683`, :gl:`!6702`, :gl:`!6728`, :gl:`!5905`, :gl:`!6762`,
:gl:`!6703`, :gl:`!6796`, :gl:`!6821`, :gl:`!6852`, :gl:`!6811`, :gl:`!6887`)

Breaking Changes
----------------

- Disable TORU. (MR :gl:`!7087`)

Ticket User-experience
----------------------

- Allow implicit accounts to receive and send tickets. (MR :gl:`!6108`, :gl:`!6490`)

RPC Changes
-----------

- Add RPC to get contract's balance of ticket with specified ticketer, content type, and content. Can be used for both implicit and originated contracts.
  ``POST /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/ticket_balance``. (MR :gl:`!6488`)

- Add RPC to get the complete list of tickets owned by a given contract by scanning the contract's storage. Can only be used for originated contracts.
  ``POST /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/all_ticket_balances``. (MR :gl:`!6712`)

Operation receipts
------------------

Cryptography
------------

- Support for BLS signatures and introduction of a new account type whose
  address has the prefix ``tz4`` (whose keys are BLS-MinPk key pairs). The
  ``CHECK_SIGNATURE`` instruction of Michelson can also check BLS
  signatures. ``tz4`` accounts are forbidden to be delegates. (MR :gl:`!5444`)

Bug Fixes
---------

15s Block Times (MR :gl:`!7017`)
--------------------------------

Blocks times have been reduced from 30 seconds to 15 seconds.
That is, a block can be produced with a delay of 15 seconds with respect to the previous block, if both blocks have round 0.
This change comes with updating many related protocol parameters in order to match the reduced block times.
In particular, the following quantities are kept the same:

- the minimal time period of a cycle (namely, 2 days, 20 hours, and 16 minutes),
- the length of the nonce revelation period (namely, around 2 hours and 8 minutes)
- the number of nonce commitments per cycle (namely, 128),
- the number of stake snapshots per cycle (namely, 16),
- the maximum rewards per minute (namely 80 tez), and therefore roughly the same inflation,
- the minimal "time to live" of an operation (namely, 1 hour),
- the block gas limit per minute (namely 10400000 gas),
- the ratio between the liquidity baking subsidy and the maximum rewards per block (namely, 1/16).

.. list-table:: Changes to protocol parameters
   :widths: 50 25 25
   :header-rows: 1

   * - Parameter (unit)
     - Old (Lima) value
     - New value
   * - ``minimal_block_delay`` (seconds)
     - ``30``
     - ``15``
   * - ``delay_increment_per_round`` (seconds)
     - ``15``
     - ``8``
   * - ``blocks_per_cycle`` (blocks)
     - ``8192``
     - ``16384``
   * - ``blocks_per_commitment`` (blocks)
     - ``64``
     - ``128``
   * - ``nonce_revelation_threshold`` (blocks)
     - ``256``
     - ``512``
   * - ``blocks_per_stake_snapshot`` (blocks)
     - ``512``
     - ``1024``
   * - ``max_operations_time_to_live`` (blocks)
     - ``120``
     - ``240``
   * - ``hard_gas_limit_per_block`` (gas unit)
     - ``5200000``
     - ``2600000``
   * - ``baking_reward_fixed_portion`` (mutez)
     - ``10000000``
     - ``5000000``
   * - ``baking_reward_bonus_per_slot`` (mutez)
     - ``4286``
     - ``2143``
   * - ``endorsing_reward_per_slot`` (mutez)
     - ``2857``
     - ``1428``
   * - ``liquidity_baking_subsidy`` (mutez)
     - ``2500000``
     - ``1250000``


Minor Changes
-------------

- Give a positive gas cost to the ``BALANCE`` instruction. (MR :gl:`!6564`)

- Enable transferring tickets to/from implicit accounts. (MRs :gl:`!6108`,
  :gl:`!6490`, :gl:`!6867`)

- Update gas for Mumbai. (MRs :gl:`!7061`, :gl:`!7299`)

Internal
--------

- Update migration for Lima. (MR :gl:`!6504`)

- Introduce local context access APIs to the indexed subcontext for optimized accesses with locality. (MR :gl:`!5922`)

- Optimized cleaning of implicit contract with 0 balance using local context accesses (MR :gl:`!5922`)

- Improve ex_ticket_size. (MR :gl:`!6209`)

- Clean up validation code. (MR :gl:`!6526`)

- Remove ``Script_typed_ir.comparable_option_t``. (MR :gl:`!6513`)

- Restrict functions to originated contracts. (MR :gl:`!6198`)

- Minor fixes for Consensus key. (MR :gl:`!6567`)

- Enrich execution traces with "just consumed gas". (MR :gl:`!6565`)

- Fix ignored values. (MRs :gl:`!6577`, :gl:`!6579`, :gl:`!6583`)

- Separate ``Generated_cost`` module. (MR :gl:`!6253`)

- Remove unused parameter in gas model for ``SPLIT_TICKET``. (MR :gl:`!6489`)

- Remove function for computing baking rights without cache update.
  (MR :gl:`!6605`)

- Move the definition of ``boxed_list`` to ``Script_list``. (MR :gl:`!5954`)

- Make ``counter`` an abstract type instead of an alias of ``Z.t``.
  (MRs :gl:`!6647`, :gl:`!6648`)

- Move interpreter logging to the plugin. (MR :gl:`!5778`)

- Use let-bindings in ticket-accounting module. (MR :gl:`!6770`)

- Use condensed syntax modules in protocol. (MRs :gl:`!6844`, :gl:`!6864`)

- Do not fully parse head fitness in ``Mempool.init``. (MR :gl:`!7000`)
