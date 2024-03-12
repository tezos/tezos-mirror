meta:
  id: protocol
  endian: be
doc: ! >-
  Encoding id: protocol

  Description: The environment a protocol relies on and the components a protocol
  is made of.
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  components:
    seq:
    - id: components_entries
      type: components_entries
      repeat: eos
  components_0:
    seq:
    - id: len_components
      type: u4
      valid:
        max: 1073741823
    - id: components
      type: components
      size: len_components
  components_entries:
    seq:
    - id: name
      type: bytes_dyn_uint30
    - id: interface_tag
      type: u1
      enum: bool
    - id: interface
      type: bytes_dyn_uint30
      if: (interface_tag == bool::true)
    - id: implementation
      type: bytes_dyn_uint30
  protocol__environment_version:
    seq:
    - id: protocol__environment_version
      type: u2
enums:
  bool:
    0: false
    255: true
seq:
- id: expected_env_version
  type: protocol__environment_version
- id: components
  type: components_0
