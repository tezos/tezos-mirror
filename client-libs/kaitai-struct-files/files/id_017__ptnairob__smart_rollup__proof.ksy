meta:
  id: id_017__ptnairob__smart_rollup__proof
  endian: be
doc: ! 'Encoding id: 017-PtNairob.smart_rollup.proof'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  dal__page__proof:
    seq:
    - id: dal_page_id
      type: dal_page_id
    - id: dal_proof
      type: bytes_dyn_uint30
  dal_page_id:
    seq:
    - id: published_level
      type: s4
    - id: slot_index
      type: u1
    - id: page_index
      type: s2
  inbox__proof:
    seq:
    - id: level
      type: s4
    - id: message_counter
      type: n
    - id: serialized_proof
      type: bytes_dyn_uint30
  input_proof:
    seq:
    - id: input_proof_tag
      type: u1
      enum: input_proof_tag
    - id: inbox__proof
      type: inbox__proof
      if: (input_proof_tag == input_proof_tag::inbox__proof)
    - id: reveal__proof
      type: reveal_proof
      if: (input_proof_tag == input_proof_tag::reveal__proof)
  n:
    seq:
    - id: n
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
  n_chunk:
    seq:
    - id: has_more
      type: b1be
    - id: payload
      type: b7be
  raw_data:
    seq:
    - id: raw_data
      size-eos: true
  raw_data_0:
    seq:
    - id: len_raw_data
      type: u2
      valid:
        max: 4096
    - id: raw_data
      type: raw_data
      size: len_raw_data
  reveal_proof:
    seq:
    - id: reveal_proof_tag
      type: u1
      enum: reveal_proof_tag
    - id: raw__data__proof
      type: raw_data_0
      if: (reveal_proof_tag == reveal_proof_tag::raw__data__proof)
    - id: dal__page__proof
      type: dal__page__proof
      if: (reveal_proof_tag == reveal_proof_tag::dal__page__proof)
enums:
  bool:
    0: false
    255: true
  input_proof_tag:
    0: inbox__proof
    1: reveal__proof
    2: first__input
  reveal_proof_tag:
    0: raw__data__proof
    1: metadata__proof
    2: dal__page__proof
seq:
- id: pvm_step
  type: bytes_dyn_uint30
- id: input_proof_tag
  type: u1
  enum: bool
- id: input_proof
  type: input_proof
  if: (input_proof_tag == bool::true)
