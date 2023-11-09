meta:
  id: alpha__vote__listings
  endian: be
doc: ! 'Encoding id: alpha.vote.listings'
types:
  alpha__vote__listings:
    seq:
    - id: alpha__vote__listings_entries
      type: alpha__vote__listings_entries
      repeat: eos
  alpha__vote__listings_entries:
    seq:
    - id: pkh
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: voting_power
      type: s8
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
enums:
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
seq:
- id: len_alpha__vote__listings
  type: s4
- id: alpha__vote__listings
  type: alpha__vote__listings
  size: len_alpha__vote__listings
