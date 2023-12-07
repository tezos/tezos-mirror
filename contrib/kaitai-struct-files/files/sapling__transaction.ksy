meta:
  id: sapling__transaction
  endian: be
  imports:
  - sapling__transaction__binding_sig
  - sapling__transaction__commitment_hash
  - sapling__transaction__input
  - sapling__transaction__output
doc: ! >-
  Encoding id: sapling.transaction

  Description: A Sapling transaction with inputs, outputs, balance, root, bound_data
  and binding sig.
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  inputs:
    seq:
    - id: inputs_entries
      type: inputs_entries
      repeat: eos
  inputs_0:
    seq:
    - id: len_inputs
      type: u4
      valid:
        max: 1833216
    - id: inputs
      type: inputs
      size: len_inputs
  inputs_entries:
    seq:
    - id: inputs_elt
      type: sapling__transaction__input
  outputs:
    seq:
    - id: outputs_entries
      type: outputs_entries
      repeat: eos
  outputs_0:
    seq:
    - id: len_outputs
      type: u4
      valid:
        max: 1073741823
    - id: outputs
      type: outputs
      size: len_outputs
  outputs_entries:
    seq:
    - id: outputs_elt
      type: sapling__transaction__output
seq:
- id: inputs
  type: inputs_0
- id: outputs
  type: outputs_0
- id: binding_sig
  type: sapling__transaction__binding_sig
- id: balance
  type: s8
- id: root
  type: sapling__transaction__commitment_hash
- id: bound_data
  type: bytes_dyn_uint30
