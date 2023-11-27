meta:
  id: id_013__ptjakart__receipt__balance_updates
  endian: be
doc: ! 'Encoding id: 013-PtJakart.receipt.balance_updates'
types:
  frozen_bonds:
    seq:
    - id: contract
      type: id_013__ptjakart__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: bond_id
      type: id_013__ptjakart__bond_id
  id_013__ptjakart__bond_id:
    seq:
    - id: id_013__ptjakart__bond_id_tag
      type: u1
      enum: id_013__ptjakart__bond_id_tag
    - id: tx_rollup_bond_id
      type: id_013__ptjakart__tx_rollup_id
      if: (id_013__ptjakart__bond_id_tag == id_013__ptjakart__bond_id_tag::tx_rollup_bond_id)
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  id_013__ptjakart__contract_id:
    seq:
    - id: id_013__ptjakart__contract_id_tag
      type: u1
      enum: id_013__ptjakart__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_013__ptjakart__contract_id_tag == id_013__ptjakart__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: originated
      type: originated
      if: (id_013__ptjakart__contract_id_tag == id_013__ptjakart__contract_id_tag::originated)
  id_013__ptjakart__operation_metadata__alpha__balance:
    seq:
    - id: id_013__ptjakart__operation_metadata__alpha__balance_tag
      type: u1
      enum: id_013__ptjakart__operation_metadata__alpha__balance_tag
    - id: contract
      type: id_013__ptjakart__contract_id
      if: (id_013__ptjakart__operation_metadata__alpha__balance_tag == id_013__ptjakart__operation_metadata__alpha__balance_tag::contract)
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: deposits
      type: public_key_hash
      if: (id_013__ptjakart__operation_metadata__alpha__balance_tag == id_013__ptjakart__operation_metadata__alpha__balance_tag::deposits)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: lost_endorsing_rewards
      type: lost_endorsing_rewards
      if: (id_013__ptjakart__operation_metadata__alpha__balance_tag == id_013__ptjakart__operation_metadata__alpha__balance_tag::lost_endorsing_rewards)
    - id: commitments
      size: 20
      if: (id_013__ptjakart__operation_metadata__alpha__balance_tag == id_013__ptjakart__operation_metadata__alpha__balance_tag::commitments)
    - id: frozen_bonds
      type: frozen_bonds
      if: (id_013__ptjakart__operation_metadata__alpha__balance_tag == id_013__ptjakart__operation_metadata__alpha__balance_tag::frozen_bonds)
  id_013__ptjakart__operation_metadata__alpha__balance_update:
    seq:
    - id: change
      type: s8
  id_013__ptjakart__operation_metadata__alpha__balance_updates:
    seq:
    - id: id_013__ptjakart__operation_metadata__alpha__balance_updates_entries
      type: id_013__ptjakart__operation_metadata__alpha__balance_updates_entries
      repeat: eos
  id_013__ptjakart__operation_metadata__alpha__balance_updates_0:
    seq:
    - id: len_id_013__ptjakart__operation_metadata__alpha__balance_updates
      type: s4
    - id: id_013__ptjakart__operation_metadata__alpha__balance_updates
      type: id_013__ptjakart__operation_metadata__alpha__balance_updates
      size: len_id_013__ptjakart__operation_metadata__alpha__balance_updates
  id_013__ptjakart__operation_metadata__alpha__balance_updates_entries:
    seq:
    - id: id_013__ptjakart__operation_metadata__alpha__balance
      type: id_013__ptjakart__operation_metadata__alpha__balance
    - id: id_013__ptjakart__operation_metadata__alpha__balance_update
      type: id_013__ptjakart__operation_metadata__alpha__balance_update
    - id: id_013__ptjakart__operation_metadata__alpha__update_origin
      type: id_013__ptjakart__operation_metadata__alpha__update_origin
  id_013__ptjakart__operation_metadata__alpha__update_origin:
    seq:
    - id: origin
      type: u1
      enum: origin_tag
  id_013__ptjakart__tx_rollup_id:
    seq:
    - id: rollup_hash
      size: 20
  lost_endorsing_rewards:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: participation
      type: u1
      enum: bool
    - id: revelation
      type: u1
      enum: bool
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
enums:
  bool:
    0: false
    255: true
  id_013__ptjakart__bond_id_tag:
    0: tx_rollup_bond_id
  id_013__ptjakart__contract_id_tag:
    0: implicit
    1: originated
  id_013__ptjakart__operation_metadata__alpha__balance_tag:
    0: contract
    2: block_fees
    4: deposits
    5: nonce_revelation_rewards
    6: double_signing_evidence_rewards
    7: endorsing_rewards
    8: baking_rewards
    9: baking_bonuses
    11: storage_fees
    12: double_signing_punishments
    13: lost_endorsing_rewards
    14: liquidity_baking_subsidies
    15: burned
    16: commitments
    17: bootstrap
    18: invoice
    19: initial_commitments
    20: minted
    21: frozen_bonds
    22: tx_rollup_rejection_rewards
    23: tx_rollup_rejection_punishments
  origin_tag:
    0: block_application
    1: protocol_migration
    2: subsidy
    3: simulation
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: id_013__ptjakart__operation_metadata__alpha__balance_updates
  type: id_013__ptjakart__operation_metadata__alpha__balance_updates_0
