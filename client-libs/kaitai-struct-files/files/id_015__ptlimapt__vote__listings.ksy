meta:
  id: id_015__ptlimapt__vote__listings
  endian: be
doc: ! 'Encoding id: 015-PtLimaPt.vote.listings'
types:
  id_015__ptlimapt__vote__listings:
    seq:
    - id: id_015__ptlimapt__vote__listings_entries
      type: id_015__ptlimapt__vote__listings_entries
      repeat: eos
  id_015__ptlimapt__vote__listings_entries:
    seq:
    - id: pkh
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: voting_power
      type: s8be
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
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: len_id_015__ptlimapt__vote__listings
  type: u4be
  valid:
    max: 1073741823
- id: id_015__ptlimapt__vote__listings
  type: id_015__ptlimapt__vote__listings
  size: len_id_015__ptlimapt__vote__listings
