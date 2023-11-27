meta:
  id: id_018__proxford__receipt__balance_updates
  endian: be
types:
  id_018__proxford__operation_metadata__alpha__balance_updates:
    seq:
    - id: len_id_018__proxford__operation_metadata__alpha__balance_updates
      type: s4
    - id: id_018__proxford__operation_metadata__alpha__balance_updates
      type: id_018__proxford__operation_metadata__alpha__balance_updates_entries
      size: len_id_018__proxford__operation_metadata__alpha__balance_updates
      repeat: eos
  id_018__proxford__operation_metadata__alpha__balance_updates_entries:
    seq:
    - id: id_018__proxford__operation_metadata__alpha__balance
      type: id_018__proxford__operation_metadata__alpha__balance
    - id: change
      type: s8
    - id: origin
      type: u1
      enum: origin_tag
  id_018__proxford__operation_metadata__alpha__balance:
    seq:
    - id: id_018__proxford__operation_metadata__alpha__balance_tag
      type: u1
      enum: id_018__proxford__operation_metadata__alpha__balance_tag
    - id: id_018__proxford__operation_metadata__alpha__balance_contract
      type: id_018__proxford__contract_id
      if: (id_018__proxford__operation_metadata__alpha__balance_tag == id_018__proxford__operation_metadata__alpha__balance_tag::contract)
    - id: id_018__proxford__operation_metadata__alpha__balance_deposits
      type: id_018__proxford__staker
      if: (id_018__proxford__operation_metadata__alpha__balance_tag == id_018__proxford__operation_metadata__alpha__balance_tag::deposits)
    - id: id_018__proxford__operation_metadata__alpha__balance_lost_attesting_rewards
      type: id_018__proxford__operation_metadata__alpha__balance_lost_attesting_rewards
      if: (id_018__proxford__operation_metadata__alpha__balance_tag == id_018__proxford__operation_metadata__alpha__balance_tag::lost_attesting_rewards)
    - id: id_018__proxford__operation_metadata__alpha__balance_commitments
      size: 20
      if: (id_018__proxford__operation_metadata__alpha__balance_tag == id_018__proxford__operation_metadata__alpha__balance_tag::commitments)
    - id: id_018__proxford__operation_metadata__alpha__balance_frozen_bonds
      type: id_018__proxford__operation_metadata__alpha__balance_frozen_bonds
      if: (id_018__proxford__operation_metadata__alpha__balance_tag == id_018__proxford__operation_metadata__alpha__balance_tag::frozen_bonds)
    - id: id_018__proxford__operation_metadata__alpha__balance_unstaked_deposits
      type: id_018__proxford__operation_metadata__alpha__balance_unstaked_deposits
      if: (id_018__proxford__operation_metadata__alpha__balance_tag == id_018__proxford__operation_metadata__alpha__balance_tag::unstaked_deposits)
  id_018__proxford__operation_metadata__alpha__balance_unstaked_deposits:
    seq:
    - id: staker
      type: id_018__proxford__staker
    - id: cycle
      type: s4
  id_018__proxford__operation_metadata__alpha__balance_frozen_bonds:
    seq:
    - id: contract
      type: id_018__proxford__contract_id
    - id: bond_id
      type: id_018__proxford__bond_id
  id_018__proxford__bond_id:
    seq:
    - id: id_018__proxford__bond_id_tag
      type: u1
      enum: id_018__proxford__bond_id_tag
    - id: id_018__proxford__bond_id_smart_rollup_bond_id
      size: 20
      if: (id_018__proxford__bond_id_tag == id_018__proxford__bond_id_tag::smart_rollup_bond_id)
  id_018__proxford__operation_metadata__alpha__balance_lost_attesting_rewards:
    seq:
    - id: delegate
      type: public_key_hash
    - id: participation
      type: u1
      enum: bool
    - id: revelation
      type: u1
      enum: bool
  id_018__proxford__staker:
    doc: ! >-
      staker: Abstract notion of staker used in operation receipts, either a single
      staker or all the stakers delegating to some delegate.
    seq:
    - id: id_018__proxford__staker_tag
      type: u1
      enum: id_018__proxford__staker_tag
    - id: id_018__proxford__staker_single
      type: id_018__proxford__staker_single
      if: (id_018__proxford__staker_tag == id_018__proxford__staker_tag::single)
    - id: id_018__proxford__staker_shared
      type: public_key_hash
      if: (id_018__proxford__staker_tag == id_018__proxford__staker_tag::shared)
  id_018__proxford__staker_single:
    seq:
    - id: contract
      type: id_018__proxford__contract_id
    - id: delegate
      type: public_key_hash
  id_018__proxford__contract_id:
    doc: ! >-
      A contract handle: A contract notation as given to an RPC or inside scripts.
      Can be a base58 implicit contract hash or a base58 originated contract hash.
    seq:
    - id: id_018__proxford__contract_id_tag
      type: u1
      enum: id_018__proxford__contract_id_tag
    - id: id_018__proxford__contract_id_implicit
      type: public_key_hash
      if: (id_018__proxford__contract_id_tag == id_018__proxford__contract_id_tag::implicit)
    - id: id_018__proxford__contract_id_originated
      type: id_018__proxford__contract_id_originated
      if: (id_018__proxford__contract_id_tag == id_018__proxford__contract_id_tag::originated)
  id_018__proxford__contract_id_originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  public_key_hash:
    doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    seq:
    - id: public_key_hash_tag
      type: u1
      enum: public_key_hash_tag
    - id: public_key_hash_ed25519
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::ed25519)
    - id: public_key_hash_secp256k1
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::secp256k1)
    - id: public_key_hash_p256
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::p256)
    - id: public_key_hash_bls
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::bls)
enums:
  origin_tag:
    0: block_application
    1: protocol_migration
    2: subsidy
    3: simulation
  id_018__proxford__bond_id_tag:
    1: smart_rollup_bond_id
  bool:
    0: false
    255: true
  id_018__proxford__staker_tag:
    0: single
    1: shared
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  id_018__proxford__contract_id_tag:
    0: implicit
    1: originated
  id_018__proxford__operation_metadata__alpha__balance_tag:
    0: contract
    2: block_fees
    4: deposits
    5: nonce_revelation_rewards
    7: attesting_rewards
    8: baking_rewards
    9: baking_bonuses
    11: storage_fees
    12: double_signing_punishments
    13: lost_attesting_rewards
    14: liquidity_baking_subsidies
    15: burned
    16: commitments
    17: bootstrap
    18: invoice
    19: initial_commitments
    20: minted
    21: frozen_bonds
    24: smart_rollup_refutation_punishments
    25: smart_rollup_refutation_rewards
    26: unstaked_deposits
seq:
- id: id_018__proxford__operation_metadata__alpha__balance_updates
  type: id_018__proxford__operation_metadata__alpha__balance_updates
