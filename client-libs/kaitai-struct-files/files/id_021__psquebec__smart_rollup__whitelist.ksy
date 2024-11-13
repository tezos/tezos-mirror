meta:
  id: id_021__psquebec__smart_rollup__whitelist
  endian: be
doc: ! 'Encoding id: 021-PsQuebec.smart_rollup.whitelist'
types:
  id_021__psquebec__smart_rollup__whitelist:
    seq:
    - id: id_021__psquebec__smart_rollup__whitelist_entries
      type: id_021__psquebec__smart_rollup__whitelist_entries
      repeat: eos
  id_021__psquebec__smart_rollup__whitelist_entries:
    seq:
    - id: signature__public_key_hash
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
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
- id: len_id_021__psquebec__smart_rollup__whitelist
  type: u4be
  valid:
    max: 1073741823
- id: id_021__psquebec__smart_rollup__whitelist
  type: id_021__psquebec__smart_rollup__whitelist
  size: len_id_021__psquebec__smart_rollup__whitelist
