meta:
  id: id_011__pthangz2__vote__listings
  endian: be
doc: ! 'Encoding id: 011-PtHangz2.vote.listings'
types:
  id_011__pthangz2__vote__listings_entries:
    seq:
    - id: pkh
      type: public_key_hash
    - id: rolls
      type: s4
  public_key_hash:
    doc: A Ed25519, Secp256k1, or P256 public key hash
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
- id: len_id_011__pthangz2__vote__listings
  type: s4
- id: id_011__pthangz2__vote__listings
  type: id_011__pthangz2__vote__listings_entries
  size: len_id_011__pthangz2__vote__listings
  repeat: eos
