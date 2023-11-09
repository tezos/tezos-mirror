meta:
  id: sapling__transaction
  endian: be
doc: ! >-
  A Sapling transaction with inputs, outputs, balance, root, bound_data and binding
  sig.
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
    - id: sapling__transaction__input
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
    - id: sapling__transaction__output
      type: sapling__transaction__output
  payload_enc:
    seq:
    - id: len_payload_enc
      type: s4
    - id: payload_enc
      size: len_payload_enc
  sapling__transaction__ciphertext:
    seq:
    - id: cv
      size: 32
    - id: epk
      size: 32
    - id: payload_enc
      type: payload_enc
    - id: nonce_enc
      size: 24
    - id: payload_out
      size: 80
    - id: nonce_out
      size: 24
  sapling__transaction__input:
    doc: Input of a transaction
    seq:
    - id: cv
      size: 32
    - id: nf
      size: 32
    - id: rk
      size: 32
    - id: proof_i
      size: 192
    - id: signature
      size: 64
  sapling__transaction__output:
    doc: Output of a transaction
    seq:
    - id: cm
      size: 32
    - id: proof_o
      size: 192
    - id: ciphertext
      type: sapling__transaction__ciphertext
seq:
- id: inputs
  type: inputs
- id: outputs
  type: outputs
- id: binding_sig
  size: 64
  doc: Binding signature of a transaction
- id: balance
  type: s8
- id: root
  size: 32
- id: bound_data
  type: bound_data
