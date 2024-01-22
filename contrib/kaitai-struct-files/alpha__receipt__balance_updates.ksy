meta:
  id: alpha__receipt__balance_updates
  endian: be
doc: ! 'Encoding id: alpha.receipt.balance_updates'
types:
  alpha__bond_id:
    seq:
    - id: alpha__bond_id_tag
      type: u1
      enum: alpha__bond_id_tag
    - id: smart_rollup_bond_id
      size: 20
      if: (alpha__bond_id_tag == alpha__bond_id_tag::smart_rollup_bond_id)
  alpha__contract_id:
    seq:
    - id: alpha__contract_id_tag
      type: u1
      enum: alpha__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (alpha__contract_id_tag == alpha__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: originated
      type: originated
      if: (alpha__contract_id_tag == alpha__contract_id_tag::originated)
  alpha__frozen_staker:
    seq:
    - id: alpha__frozen_staker_tag
      type: u1
      enum: alpha__frozen_staker_tag
    - id: single
      type: single
      if: (alpha__frozen_staker_tag == alpha__frozen_staker_tag::single)
    - id: shared
      type: public_key_hash
      if: (alpha__frozen_staker_tag == alpha__frozen_staker_tag::shared)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: baker
      type: public_key_hash
      if: (alpha__frozen_staker_tag == alpha__frozen_staker_tag::baker)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  alpha__operation_metadata__alpha__balance_and_update:
    seq:
    - id: alpha__operation_metadata__alpha__balance_and_update_tag
      type: u1
      enum: alpha__operation_metadata__alpha__balance_and_update_tag
    - id: contract
      type: contract
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::contract)
    - id: block_fees
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::block_fees)
    - id: deposits
      type: deposits
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::deposits)
    - id: nonce_revelation_rewards
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::nonce_revelation_rewards)
    - id: attesting_rewards
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::attesting_rewards)
    - id: baking_rewards
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::baking_rewards)
    - id: baking_bonuses
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::baking_bonuses)
    - id: storage_fees
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::storage_fees)
    - id: double_signing_punishments
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::double_signing_punishments)
    - id: lost_attesting_rewards
      type: lost_attesting_rewards
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::lost_attesting_rewards)
    - id: liquidity_baking_subsidies
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::liquidity_baking_subsidies)
    - id: burned
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::burned)
    - id: commitments
      type: commitments
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::commitments)
    - id: bootstrap
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::bootstrap)
    - id: invoice
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::invoice)
    - id: initial_commitments
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::initial_commitments)
    - id: minted
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::minted)
    - id: frozen_bonds
      type: frozen_bonds
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::frozen_bonds)
    - id: smart_rollup_refutation_punishments
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::smart_rollup_refutation_punishments)
    - id: smart_rollup_refutation_rewards
      type: alpha__operation_metadata__alpha__tez_balance_update
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::smart_rollup_refutation_rewards)
    - id: unstaked_deposits
      type: unstaked_deposits
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::unstaked_deposits)
    - id: staking_delegator_numerator
      type: staking_delegator_numerator
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::staking_delegator_numerator)
    - id: staking_delegate_denominator
      type: staking_delegate_denominator
      if: (alpha__operation_metadata__alpha__balance_and_update_tag == alpha__operation_metadata__alpha__balance_and_update_tag::staking_delegate_denominator)
  alpha__operation_metadata__alpha__balance_updates:
    seq:
    - id: alpha__operation_metadata__alpha__balance_updates_entries
      type: alpha__operation_metadata__alpha__balance_updates_entries
      repeat: eos
  alpha__operation_metadata__alpha__balance_updates_0:
    seq:
    - id: len_alpha__operation_metadata__alpha__balance_updates
      type: u4
      valid:
        max: 1073741823
    - id: alpha__operation_metadata__alpha__balance_updates
      type: alpha__operation_metadata__alpha__balance_updates
      size: len_alpha__operation_metadata__alpha__balance_updates
  alpha__operation_metadata__alpha__balance_updates_entries:
    seq:
    - id: alpha__operation_metadata__alpha__balance_and_update
      type: alpha__operation_metadata__alpha__balance_and_update
    - id: alpha__operation_metadata__alpha__update_origin
      type: alpha__operation_metadata__alpha__update_origin
  alpha__operation_metadata__alpha__staking_abstract_quantity:
    seq:
    - id: change
      type: s8
  alpha__operation_metadata__alpha__tez_balance_update:
    seq:
    - id: change
      type: s8
  alpha__operation_metadata__alpha__update_origin:
    seq:
    - id: alpha__operation_metadata__alpha__update_origin_tag
      type: u1
      enum: alpha__operation_metadata__alpha__update_origin_tag
    - id: delayed_operation
      size: 32
      if: (alpha__operation_metadata__alpha__update_origin_tag == alpha__operation_metadata__alpha__update_origin_tag::delayed_operation)
  alpha__staker:
    seq:
    - id: alpha__staker_tag
      type: u1
      enum: alpha__staker_tag
    - id: single
      type: single
      if: (alpha__staker_tag == alpha__staker_tag::single)
    - id: shared
      type: public_key_hash
      if: (alpha__staker_tag == alpha__staker_tag::shared)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  commitments:
    seq:
    - id: committer
      size: 20
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  contract:
    seq:
    - id: contract
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  deposits:
    seq:
    - id: staker
      type: alpha__frozen_staker
      doc: ! >-
        frozen_staker: Abstract notion of staker used in operation receipts for frozen
        deposits, either a single staker or all the stakers delegating to some delegate.
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  frozen_bonds:
    seq:
    - id: contract
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: bond_id
      type: alpha__bond_id
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  lost_attesting_rewards:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: participation
      type: u1
      enum: bool
    - id: revelation
      type: u1
      enum: bool
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
  originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  public_key_hash:
    seq:
    - id: public_key_hash_tag
      type: u1
      enum: public_key_hash_tag
    - id: ed25519
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::ed25519)
    - id: secp256k1
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::secp256k1)
    - id: p256
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::p256)
    - id: bls
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::bls)
  single:
    seq:
    - id: contract
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  staking_delegate_denominator:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: alpha__operation_metadata__alpha__staking_abstract_quantity
      type: alpha__operation_metadata__alpha__staking_abstract_quantity
  staking_delegator_numerator:
    seq:
    - id: delegator
      type: alpha__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: alpha__operation_metadata__alpha__staking_abstract_quantity
      type: alpha__operation_metadata__alpha__staking_abstract_quantity
  unstaked_deposits:
    seq:
    - id: staker
      type: alpha__staker
      doc: ! >-
        unstaked_frozen_staker: Abstract notion of staker used in operation receipts
        for unstaked frozen deposits, either a single staker or all the stakers delegating
        to some delegate.
    - id: cycle
      type: s4
    - id: alpha__operation_metadata__alpha__tez_balance_update
      type: alpha__operation_metadata__alpha__tez_balance_update
enums:
  alpha__bond_id_tag:
    1: smart_rollup_bond_id
  alpha__contract_id_tag:
    0: implicit
    1: originated
  alpha__frozen_staker_tag:
    0: single
    1: shared
    2: baker
  alpha__operation_metadata__alpha__balance_and_update_tag:
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
    27: staking_delegator_numerator
    28: staking_delegate_denominator
  alpha__operation_metadata__alpha__update_origin_tag:
    0: block_application
    1: protocol_migration
    2: subsidy
    3: simulation
    4: delayed_operation
  alpha__staker_tag:
    0: single
    1: shared
  bool:
    0: false
    255: true
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
seq:
- id: alpha__operation_metadata__alpha__balance_updates
  type: alpha__operation_metadata__alpha__balance_updates_0
