meta:
  id: id_024__pttallin__smart_rollup__reveal
  endian: be
doc: ! 'Encoding id: 024-PtTALLiN.smart_rollup.reveal'
types:
  id_024__pttallin__contract_id:
    seq:
    - id: id_024__pttallin__contract_id_tag
      type: u1
      enum: id_024__pttallin__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_024__pttallin__contract_id_tag == id_024__pttallin__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: originated
      type: originated
      if: (id_024__pttallin__contract_id_tag == id_024__pttallin__contract_id_tag::originated)
  input_hash:
    seq:
    - id: input_hash_tag
      type: u1
      enum: input_hash_tag
    - id: reveal_data_hash_v0
      size: 32
      if: (input_hash_tag == input_hash_tag::reveal_data_hash_v0)
  originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  page_id:
    seq:
    - id: published_level
      type: s4be
    - id: slot_index
      type: u1
    - id: page_index
      type: s2be
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
  request_adaptive_dal_page:
    seq:
    - id: page_id
      type: page_id
    - id: attestation_threshold_percent
      type: u1
    - id: restricted_commitments_publishers
      type: restricted_commitments_publishers
  restricted_commitments_publishers:
    seq:
    - id: restricted_commitments_publishers_tag
      type: u1
      enum: restricted_commitments_publishers_tag
    - id: some
      type: some_0
      if: (restricted_commitments_publishers_tag == restricted_commitments_publishers_tag::some)
  some:
    seq:
    - id: some_entries
      type: some_entries
      repeat: eos
  some_0:
    seq:
    - id: len_some
      type: u4be
      valid:
        max: 220
    - id: some
      type: some
      size: len_some
  some_entries:
    seq:
    - id: id_024__pttallin__contract_id
      type: id_024__pttallin__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
enums:
  id_024__pttallin__contract_id_tag:
    0: implicit
    1: originated
  id_024__pttallin__smart_rollup__reveal_tag:
    0: reveal_raw_data
    1: reveal_metadata
    2: request_dal_page
    3: reveal_dal_parameters
    4: request_adaptive_dal_page
  input_hash_tag:
    0: reveal_data_hash_v0
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  restricted_commitments_publishers_tag:
    0: none
    1: some
seq:
- id: id_024__pttallin__smart_rollup__reveal_tag
  type: u1
  enum: id_024__pttallin__smart_rollup__reveal_tag
- id: reveal_raw_data
  type: input_hash
  if: (id_024__pttallin__smart_rollup__reveal_tag == id_024__pttallin__smart_rollup__reveal_tag::reveal_raw_data)
- id: request_dal_page
  type: page_id
  if: (id_024__pttallin__smart_rollup__reveal_tag == id_024__pttallin__smart_rollup__reveal_tag::request_dal_page)
- id: request_adaptive_dal_page
  type: request_adaptive_dal_page
  if: (id_024__pttallin__smart_rollup__reveal_tag == id_024__pttallin__smart_rollup__reveal_tag::request_adaptive_dal_page)
