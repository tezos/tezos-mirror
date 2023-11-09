meta:
  id: id_006__pscartha__delegate__balance_updates
  endian: be
doc: ! 'Encoding id: 006-PsCARTHA.delegate.balance_updates'
types:
  deposits:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: cycle
      type: s4
  fees:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: cycle
      type: s4
  id_006__pscartha__contract_id:
    seq:
    - id: id_006__pscartha__contract_id_tag
      type: u1
      enum: id_006__pscartha__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_006__pscartha__contract_id_tag == id_006__pscartha__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: originated
      type: originated
      if: (id_006__pscartha__contract_id_tag == id_006__pscartha__contract_id_tag::originated)
  id_006__pscartha__operation_metadata__alpha__balance:
    seq:
    - id: id_006__pscartha__operation_metadata__alpha__balance_tag
      type: u1
      enum: id_006__pscartha__operation_metadata__alpha__balance_tag
    - id: contract
      type: id_006__pscartha__contract_id
      if: (id_006__pscartha__operation_metadata__alpha__balance_tag == id_006__pscartha__operation_metadata__alpha__balance_tag::contract)
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: rewards
      type: rewards
      if: (id_006__pscartha__operation_metadata__alpha__balance_tag == id_006__pscartha__operation_metadata__alpha__balance_tag::rewards)
    - id: fees
      type: fees
      if: (id_006__pscartha__operation_metadata__alpha__balance_tag == id_006__pscartha__operation_metadata__alpha__balance_tag::fees)
    - id: deposits
      type: deposits
      if: (id_006__pscartha__operation_metadata__alpha__balance_tag == id_006__pscartha__operation_metadata__alpha__balance_tag::deposits)
  id_006__pscartha__operation_metadata__alpha__balance_update:
    seq:
    - id: change
      type: s8
  id_006__pscartha__operation_metadata__alpha__balance_updates:
    seq:
    - id: id_006__pscartha__operation_metadata__alpha__balance_updates_entries
      type: id_006__pscartha__operation_metadata__alpha__balance_updates_entries
      repeat: eos
  id_006__pscartha__operation_metadata__alpha__balance_updates_0:
    seq:
    - id: len_id_006__pscartha__operation_metadata__alpha__balance_updates
      type: s4
    - id: id_006__pscartha__operation_metadata__alpha__balance_updates
      type: id_006__pscartha__operation_metadata__alpha__balance_updates
      size: len_id_006__pscartha__operation_metadata__alpha__balance_updates
  id_006__pscartha__operation_metadata__alpha__balance_updates_entries:
    seq:
    - id: id_006__pscartha__operation_metadata__alpha__balance
      type: id_006__pscartha__operation_metadata__alpha__balance
    - id: id_006__pscartha__operation_metadata__alpha__balance_update
      type: id_006__pscartha__operation_metadata__alpha__balance_update
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
  rewards:
    seq:
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: cycle
      type: s4
enums:
  id_006__pscartha__contract_id_tag:
    0: implicit
    1: originated
  id_006__pscartha__operation_metadata__alpha__balance_tag:
    0: contract
    1: rewards
    2: fees
    3: deposits
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: id_006__pscartha__operation_metadata__alpha__balance_updates
  type: id_006__pscartha__operation_metadata__alpha__balance_updates_0
