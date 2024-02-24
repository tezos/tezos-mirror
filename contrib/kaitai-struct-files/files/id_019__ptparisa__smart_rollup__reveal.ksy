meta:
  id: id_019__ptparisa__smart_rollup__reveal
  endian: be
doc: ! 'Encoding id: 019-PtParisA.smart_rollup.reveal'
types:
  input_hash:
    seq:
    - id: input_hash_tag
      type: u1
      enum: input_hash_tag
    - id: reveal_data_hash_v0
      size: 32
      if: (input_hash_tag == input_hash_tag::reveal_data_hash_v0)
  page_id:
    seq:
    - id: published_level
      type: s4
    - id: slot_index
      type: u1
    - id: page_index
      type: s2
enums:
  id_019__ptparisa__smart_rollup__reveal_tag:
    0: reveal_raw_data
    1: reveal_metadata
    2: request_dal_page
    3: reveal_dal_parameters
  input_hash_tag:
    0: reveal_data_hash_v0
seq:
- id: id_019__ptparisa__smart_rollup__reveal_tag
  type: u1
  enum: id_019__ptparisa__smart_rollup__reveal_tag
- id: reveal_raw_data
  type: input_hash
  if: (id_019__ptparisa__smart_rollup__reveal_tag == id_019__ptparisa__smart_rollup__reveal_tag::reveal_raw_data)
- id: request_dal_page
  type: page_id
  if: (id_019__ptparisa__smart_rollup__reveal_tag == id_019__ptparisa__smart_rollup__reveal_tag::request_dal_page)
