meta:
  id: id_016__ptmumbai__contract
  endian: be
doc: ! 'Encoding id: 016-PtMumbai.contract'
types:
  id_016__ptmumbai__contract_id:
    doc: ! >-
      A contract handle: A contract notation as given to an RPC or inside scripts.
      Can be a base58 implicit contract hash or a base58 originated contract hash.
    seq:
    - id: id_016__ptmumbai__contract_id_tag
      type: u1
      enum: id_016__ptmumbai__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_016__ptmumbai__contract_id_tag == id_016__ptmumbai__contract_id_tag::implicit)
    - id: originated
      type: originated
      if: (id_016__ptmumbai__contract_id_tag == id_016__ptmumbai__contract_id_tag::originated)
  originated:
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
  id_016__ptmumbai__contract_id_tag:
    0: implicit
    1: originated
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
seq:
- id: id_016__ptmumbai__contract_id
  type: id_016__ptmumbai__contract_id
