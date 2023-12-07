meta:
  id: signer_messages__request
  endian: be
doc: ! 'Encoding id: signer_messages.request'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  deterministic_nonce:
    seq:
    - id: pkh
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: data
      type: bytes_dyn_uint30
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size-eos: true
      if: (signature_tag == bool::true)
  deterministic_nonce_hash:
    seq:
    - id: pkh
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: data
      type: bytes_dyn_uint30
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size-eos: true
      if: (signature_tag == bool::true)
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
  sign:
    seq:
    - id: pkh
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: data
      type: bytes_dyn_uint30
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size-eos: true
      if: (signature_tag == bool::true)
  signer_messages__public_key__request:
    seq:
    - id: pkh
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  signer_messages__supports_deterministic_nonces__request:
    seq:
    - id: pkh
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
enums:
  bool:
    0: false
    255: true
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  signer_messages__request_tag:
    0: sign
    1: public_key
    2: authorized_keys
    3: deterministic_nonce
    4: deterministic_nonce_hash
    5: supports_deterministic_nonces
seq:
- id: signer_messages__request_tag
  type: u1
  enum: signer_messages__request_tag
- id: sign
  type: sign
  if: (signer_messages__request_tag == signer_messages__request_tag::sign)
- id: public_key
  type: signer_messages__public_key__request
  if: (signer_messages__request_tag == signer_messages__request_tag::public_key)
- id: deterministic_nonce
  type: deterministic_nonce
  if: (signer_messages__request_tag == signer_messages__request_tag::deterministic_nonce)
- id: deterministic_nonce_hash
  type: deterministic_nonce_hash
  if: (signer_messages__request_tag == signer_messages__request_tag::deterministic_nonce_hash)
- id: supports_deterministic_nonces
  type: signer_messages__supports_deterministic_nonces__request
  if: (signer_messages__request_tag == signer_messages__request_tag::supports_deterministic_nonces)
