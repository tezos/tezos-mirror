meta:
  id: id_008__ptedo2zk__delegate__balance_updates
  endian: be
types:
  id_008__ptedo2zk__operation_metadata__alpha__balance_updates:
    seq:
    - id: len_id_008__ptedo2zk__operation_metadata__alpha__balance_updates
      type: s4
    - id: id_008__ptedo2zk__operation_metadata__alpha__balance_updates
      type: id_008__ptedo2zk__operation_metadata__alpha__balance_updates_entries
      size: len_id_008__ptedo2zk__operation_metadata__alpha__balance_updates
      repeat: eos
  id_008__ptedo2zk__operation_metadata__alpha__balance_updates_entries:
    seq:
    - id: id_008__ptedo2zk__operation_metadata__alpha__balance
      type: id_008__ptedo2zk__operation_metadata__alpha__balance
    - id: change
      type: s8
  id_008__ptedo2zk__operation_metadata__alpha__balance:
    seq:
    - id: id_008__ptedo2zk__operation_metadata__alpha__balance_tag
      type: u1
      enum: id_008__ptedo2zk__operation_metadata__alpha__balance_tag
    - id: id_008__ptedo2zk__operation_metadata__alpha__balance_contract
      type: id_008__ptedo2zk__contract_id
      if: (id_008__ptedo2zk__operation_metadata__alpha__balance_tag == id_008__ptedo2zk__operation_metadata__alpha__balance_tag::contract)
    - id: id_008__ptedo2zk__operation_metadata__alpha__balance_rewards
      type: id_008__ptedo2zk__operation_metadata__alpha__balance_rewards
      if: (id_008__ptedo2zk__operation_metadata__alpha__balance_tag == id_008__ptedo2zk__operation_metadata__alpha__balance_tag::rewards)
    - id: id_008__ptedo2zk__operation_metadata__alpha__balance_fees
      type: id_008__ptedo2zk__operation_metadata__alpha__balance_fees
      if: (id_008__ptedo2zk__operation_metadata__alpha__balance_tag == id_008__ptedo2zk__operation_metadata__alpha__balance_tag::fees)
    - id: id_008__ptedo2zk__operation_metadata__alpha__balance_deposits
      type: id_008__ptedo2zk__operation_metadata__alpha__balance_deposits
      if: (id_008__ptedo2zk__operation_metadata__alpha__balance_tag == id_008__ptedo2zk__operation_metadata__alpha__balance_tag::deposits)
  id_008__ptedo2zk__operation_metadata__alpha__balance_deposits:
    seq:
    - id: delegate
      type: public_key_hash
    - id: cycle
      type: s4
  id_008__ptedo2zk__operation_metadata__alpha__balance_fees:
    seq:
    - id: delegate
      type: public_key_hash
    - id: cycle
      type: s4
  id_008__ptedo2zk__operation_metadata__alpha__balance_rewards:
    seq:
    - id: delegate
      type: public_key_hash
    - id: cycle
      type: s4
  id_008__ptedo2zk__contract_id:
    doc: ! >-
      A contract handle: A contract notation as given to an RPC or inside scripts.
      Can be a base58 implicit contract hash or a base58 originated contract hash.
    seq:
    - id: id_008__ptedo2zk__contract_id_tag
      type: u1
      enum: id_008__ptedo2zk__contract_id_tag
    - id: id_008__ptedo2zk__contract_id_implicit
      type: public_key_hash
      if: (id_008__ptedo2zk__contract_id_tag == id_008__ptedo2zk__contract_id_tag::implicit)
    - id: id_008__ptedo2zk__contract_id_originated
      type: id_008__ptedo2zk__contract_id_originated
      if: (id_008__ptedo2zk__contract_id_tag == id_008__ptedo2zk__contract_id_tag::originated)
  id_008__ptedo2zk__contract_id_originated:
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
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  id_008__ptedo2zk__contract_id_tag:
    0: implicit
    1: originated
  id_008__ptedo2zk__operation_metadata__alpha__balance_tag:
    0: contract
    1: rewards
    2: fees
    3: deposits
seq:
- id: id_008__ptedo2zk__operation_metadata__alpha__balance_updates
  type: id_008__ptedo2zk__operation_metadata__alpha__balance_updates
