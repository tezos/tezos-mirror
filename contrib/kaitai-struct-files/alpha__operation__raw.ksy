meta:
  id: alpha__operation__raw
  endian: be
types:
  operation:
    doc: ! >-
      An operation. The shell_header part indicates a block an operation is meant
      to apply on top of. The proto part is protocol-specific and appears as a binary
      blob.
    seq:
    - id: branch
      size: 32
      doc: An operation's shell header.
    - id: data
      size-eos: true
seq:
- id: operation
  type: operation