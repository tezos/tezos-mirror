meta:
  id: signer_messages__known_keys__response
  endian: be
doc: ! 'Encoding id: signer_messages.known_keys.response'
types:
  known_keys:
    seq:
    - id: known_keys_entries
      type: known_keys_entries
      repeat: eos
  known_keys_0:
    seq:
    - id: len_known_keys
      type: u4be
      valid:
        max: 1073741823
    - id: known_keys
      type: known_keys
      size: len_known_keys
  known_keys_entries:
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
- id: known_keys
  type: known_keys_0
