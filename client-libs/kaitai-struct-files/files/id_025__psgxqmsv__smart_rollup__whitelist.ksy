meta:
  id: id_025__psgxqmsv__smart_rollup__whitelist
  endian: be
doc: ! 'Encoding id: 025-PsGXQmSv.smart_rollup.whitelist'
types:
  id_025__psgxqmsv__smart_rollup__whitelist:
    seq:
    - id: id_025__psgxqmsv__smart_rollup__whitelist_entries
      type: id_025__psgxqmsv__smart_rollup__whitelist_entries
      repeat: eos
  id_025__psgxqmsv__smart_rollup__whitelist_entries:
    seq:
    - id: signature__public_key_hash
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, BLS or Mldsa44 public key hash
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
    - id: mldsa44
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::mldsa44)
enums:
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
    4: mldsa44
seq:
- id: len_id_025__psgxqmsv__smart_rollup__whitelist
  type: u4be
  valid:
    max: 1073741823
- id: id_025__psgxqmsv__smart_rollup__whitelist
  type: id_025__psgxqmsv__smart_rollup__whitelist
  size: len_id_025__psgxqmsv__smart_rollup__whitelist
