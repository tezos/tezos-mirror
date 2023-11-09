meta:
  id: operation
  endian: be
doc: ! >-
  Encoding id: operation

  Description: An operation. The shell_header part indicates a block an operation
  is meant to apply on top of. The proto part is protocol-specific and appears as
  a binary blob.
seq:
- id: branch
  size: 32
  doc: An operation's shell header.
- id: data
  size-eos: true
