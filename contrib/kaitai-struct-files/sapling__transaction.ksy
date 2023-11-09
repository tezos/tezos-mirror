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
  bound_data:
    seq:
    - id: len_bound_data
      type: s4
    - id: bound_data
      size: len_bound_data
  inputs:
    seq:
    - id: len_inputs
      type: s4
    - id: inputs
      type: inputs_entries
      size: len_inputs
      repeat: eos
  inputs_entries:
    seq:
    - id: inputs_elt
      type: sapling__transaction__input
  outputs:
    seq:
    - id: len_outputs
      type: s4
    - id: outputs
      type: outputs_entries
      size: len_outputs
      repeat: eos
  outputs_entries:
    seq:
    - id: outputs_elt
      type: sapling__transaction__output
seq:
- id: inputs
  type: inputs
- id: outputs
  type: outputs
- id: binding_sig
  type: sapling__transaction__binding_sig
- id: balance
  type: s8
- id: root
  type: sapling__transaction__commitment_hash
- id: bound_data
  type: bound_data
