meta:
  id: alpha__frozen_staker
  endian: be
doc: ! 'Encoding id: alpha.frozen_staker'
types:
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
enums:
  alpha__contract_id_tag:
    0: implicit
    1: originated
  alpha__frozen_staker_tag:
    0: single
    1: shared
    2: baker
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
seq:
- id: alpha__frozen_staker
  type: alpha__frozen_staker
  doc: ! >-
    frozen_staker: Abstract notion of staker used in operation receipts for frozen
    deposits, either a single staker or all the stakers delegating to some delegate.
