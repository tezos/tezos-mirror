meta:
  id: alpha__unstaked_frozen_staker
  endian: be
types:
  alpha__staker:
    doc: ! >-
      unstaked_frozen_staker: Abstract notion of staker used in operation receipts
      for unstaked frozen deposits, either a single staker or all the stakers delegating
      to some delegate.
    seq:
    - id: alpha__staker_tag
      type: u1
      enum: alpha__staker_tag
    - id: alpha__staker_single
      type: alpha__staker_single
      if: (alpha__staker_tag == alpha__staker_tag::single)
    - id: alpha__staker_shared
      type: public_key_hash
      if: (alpha__staker_tag == alpha__staker_tag::shared)
  alpha__staker_single:
    seq:
    - id: contract
      type: alpha__contract_id
    - id: delegate
      type: public_key_hash
  alpha__contract_id:
    doc: ! >-
      A contract handle: A contract notation as given to an RPC or inside scripts.
      Can be a base58 implicit contract hash or a base58 originated contract hash.
    seq:
    - id: alpha__contract_id_tag
      type: u1
      enum: alpha__contract_id_tag
    - id: alpha__contract_id_implicit
      type: public_key_hash
      if: (alpha__contract_id_tag == alpha__contract_id_tag::implicit)
    - id: alpha__contract_id_originated
      type: alpha__contract_id_originated
      if: (alpha__contract_id_tag == alpha__contract_id_tag::originated)
  alpha__contract_id_originated:
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
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  alpha__contract_id_tag:
    0: implicit
    1: originated
  alpha__staker_tag:
    0: single
    1: shared
seq:
- id: alpha__staker
  type: alpha__staker
