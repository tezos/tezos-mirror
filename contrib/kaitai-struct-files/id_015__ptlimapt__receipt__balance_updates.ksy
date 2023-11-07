meta:
  id: id_015__ptlimapt__receipt__balance_updates
  endian: be
types:
  id_015__ptlimapt__operation_metadata__alpha__balance_updates:
    seq:
    - id: len_id_015__ptlimapt__operation_metadata__alpha__balance_updates
      type: s4
    - id: id_015__ptlimapt__operation_metadata__alpha__balance_updates
      type: id_015__ptlimapt__operation_metadata__alpha__balance_updates_entries
      size: len_id_015__ptlimapt__operation_metadata__alpha__balance_updates
      repeat: eos
  id_015__ptlimapt__operation_metadata__alpha__balance_updates_entries:
    seq:
    - id: id_015__ptlimapt__operation_metadata__alpha__balance
      type: id_015__ptlimapt__operation_metadata__alpha__balance
    - id: change
      type: s8
    - id: origin
      type: u1
      enum: origin_tag
  id_015__ptlimapt__operation_metadata__alpha__balance:
    seq:
    - id: id_015__ptlimapt__operation_metadata__alpha__balance_tag
      type: u1
      enum: id_015__ptlimapt__operation_metadata__alpha__balance_tag
    - id: id_015__ptlimapt__operation_metadata__alpha__balance_contract
      type: id_015__ptlimapt__contract_id
      if: (id_015__ptlimapt__operation_metadata__alpha__balance_tag == id_015__ptlimapt__operation_metadata__alpha__balance_tag::contract)
    - id: id_015__ptlimapt__operation_metadata__alpha__balance_deposits
      type: public_key_hash
      if: (id_015__ptlimapt__operation_metadata__alpha__balance_tag == id_015__ptlimapt__operation_metadata__alpha__balance_tag::deposits)
    - id: id_015__ptlimapt__operation_metadata__alpha__balance_lost_endorsing_rewards
      type: id_015__ptlimapt__operation_metadata__alpha__balance_lost_endorsing_rewards
      if: (id_015__ptlimapt__operation_metadata__alpha__balance_tag == id_015__ptlimapt__operation_metadata__alpha__balance_tag::lost_endorsing_rewards)
    - id: id_015__ptlimapt__operation_metadata__alpha__balance_commitments
      size: 20
      if: (id_015__ptlimapt__operation_metadata__alpha__balance_tag == id_015__ptlimapt__operation_metadata__alpha__balance_tag::commitments)
    - id: id_015__ptlimapt__operation_metadata__alpha__balance_frozen_bonds
      type: id_015__ptlimapt__operation_metadata__alpha__balance_frozen_bonds
      if: (id_015__ptlimapt__operation_metadata__alpha__balance_tag == id_015__ptlimapt__operation_metadata__alpha__balance_tag::frozen_bonds)
  id_015__ptlimapt__operation_metadata__alpha__balance_frozen_bonds:
    seq:
    - id: contract
      type: id_015__ptlimapt__contract_id
    - id: bond_id
      type: id_015__ptlimapt__bond_id
  id_015__ptlimapt__bond_id:
    seq:
    - id: id_015__ptlimapt__bond_id_tag
      type: u1
      enum: id_015__ptlimapt__bond_id_tag
    - id: id_015__ptlimapt__bond_id_tx_rollup_bond_id
      size: 20
      if: (id_015__ptlimapt__bond_id_tag == id_015__ptlimapt__bond_id_tag::tx_rollup_bond_id)
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: id_015__ptlimapt__bond_id_sc_rollup_bond_id
      type: id_015__ptlimapt__rollup_address
      if: (id_015__ptlimapt__bond_id_tag == id_015__ptlimapt__bond_id_tag::sc_rollup_bond_id)
  id_015__ptlimapt__rollup_address:
    doc: ! >-
      A smart contract rollup address: A smart contract rollup is identified by a
      base58 address starting with scr1
    seq:
    - id: len_id_015__ptlimapt__rollup_address
      type: s4
    - id: id_015__ptlimapt__rollup_address
      size: len_id_015__ptlimapt__rollup_address
  id_015__ptlimapt__operation_metadata__alpha__balance_lost_endorsing_rewards:
    seq:
    - id: delegate
      type: public_key_hash
    - id: participation
      type: u1
      enum: bool
    - id: revelation
      type: u1
      enum: bool
  id_015__ptlimapt__contract_id:
    doc: ! >-
      A contract handle: A contract notation as given to an RPC or inside scripts.
      Can be a base58 implicit contract hash or a base58 originated contract hash.
    seq:
    - id: id_015__ptlimapt__contract_id_tag
      type: u1
      enum: id_015__ptlimapt__contract_id_tag
    - id: id_015__ptlimapt__contract_id_implicit
      type: public_key_hash
      if: (id_015__ptlimapt__contract_id_tag == id_015__ptlimapt__contract_id_tag::implicit)
    - id: id_015__ptlimapt__contract_id_originated
      type: id_015__ptlimapt__contract_id_originated
      if: (id_015__ptlimapt__contract_id_tag == id_015__ptlimapt__contract_id_tag::originated)
  id_015__ptlimapt__contract_id_originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  public_key_hash:
    doc: A Ed25519, Secp256k1, or P256 public key hash
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
enums:
  origin_tag:
    0: block_application
    1: protocol_migration
    2: subsidy
    3: simulation
  id_015__ptlimapt__bond_id_tag:
    0: tx_rollup_bond_id
    1: sc_rollup_bond_id
  bool:
    0: false
    255: true
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  id_015__ptlimapt__contract_id_tag:
    0: implicit
    1: originated
  id_015__ptlimapt__operation_metadata__alpha__balance_tag:
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
    24: sc_rollup_refutation_punishments
    25: sc_rollup_refutation_rewards
seq:
- id: id_015__ptlimapt__operation_metadata__alpha__balance_updates
  type: id_015__ptlimapt__operation_metadata__alpha__balance_updates
