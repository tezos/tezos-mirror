meta:
  id: operation
  endian: be
  imports:
  - operation__shell_header
doc: ! >-
  Encoding id: operation

  Description: An operation. The shell_header part indicates a block an operation
  is meant to apply on top of. The proto part is protocol-specific and appears as
  a binary blob.
seq:
- id: operation
  type: operation__shell_header
- id: data
  size-eos: true
