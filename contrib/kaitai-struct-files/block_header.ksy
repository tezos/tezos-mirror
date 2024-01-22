meta:
  id: block_header
  endian: be
  imports:
  - block_header__shell
doc: ! >-
  Encoding id: block_header

  Description: Block header. It contains both shell and protocol specific data.
seq:
- id: block_header
  type: block_header__shell
- id: protocol_data
  size-eos: true
