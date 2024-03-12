meta:
  id: id_019__ptparisa__fa1__2__token_transfer
  endian: be
doc: ! 'Encoding id: 019-PtParisA.fa1.2.token_transfer'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
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
  z:
    seq:
    - id: has_tail
      type: b1be
    - id: sign
      type: b1be
    - id: payload
      type: b6be
    - id: tail
      type: n_chunk
      repeat: until
      repeat-until: not (_.has_more).as<bool>
      if: has_tail.as<bool>
enums:
  bool:
    0: false
    255: true
seq:
- id: token_contract
  type: bytes_dyn_uint30
- id: destination
  type: bytes_dyn_uint30
- id: amount
  type: z
- id: tez__amount_tag
  type: u1
  enum: bool
- id: tez__amount
  type: bytes_dyn_uint30
  if: (tez__amount_tag == bool::true)
- id: fee_tag
  type: u1
  enum: bool
- id: fee
  type: bytes_dyn_uint30
  if: (fee_tag == bool::true)
- id: gas__limit_tag
  type: u1
  enum: bool
- id: gas__limit
  type: n
  if: (gas__limit_tag == bool::true)
- id: storage__limit_tag
  type: u1
  enum: bool
- id: storage__limit
  type: z
  if: (storage__limit_tag == bool::true)
