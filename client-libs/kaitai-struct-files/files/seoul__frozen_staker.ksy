meta:
  id: seoul__frozen_staker
  endian: be
doc: ! 'Encoding id: seoul.frozen_staker'
types:
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
  seoul__contract_id:
    seq:
    - id: seoul__contract_id_tag
      type: u1
      enum: seoul__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (seoul__contract_id_tag == seoul__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: originated
      type: originated
      if: (seoul__contract_id_tag == seoul__contract_id_tag::originated)
  seoul__frozen_staker:
    seq:
    - id: seoul__frozen_staker_tag
      type: u1
      enum: seoul__frozen_staker_tag
    - id: single
      type: single
      if: (seoul__frozen_staker_tag == seoul__frozen_staker_tag::single)
    - id: shared
      type: public_key_hash
      if: (seoul__frozen_staker_tag == seoul__frozen_staker_tag::shared)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: baker
      type: public_key_hash
      if: (seoul__frozen_staker_tag == seoul__frozen_staker_tag::baker)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: baker_edge
      type: public_key_hash
      if: (seoul__frozen_staker_tag == seoul__frozen_staker_tag::baker_edge)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  single:
    seq:
    - id: contract
      type: seoul__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
enums:
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  seoul__contract_id_tag:
    0: implicit
    1: originated
  seoul__frozen_staker_tag:
    0: single
    1: shared
    2: baker
    3: baker_edge
seq:
- id: seoul__frozen_staker
  type: seoul__frozen_staker
  doc: ! >-
    frozen_staker: Abstract notion of staker used in operation receipts for frozen
    deposits, either a single staker or all the stakers delegating to some delegate.
