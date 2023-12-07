meta:
  id: protocol__meta
  endian: be
doc: ! >-
  Encoding id: protocol.meta

  Description: Protocol metadata: the hash of the protocol, the expected environment
  version and the list of modules comprising the protocol.
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  modules:
    seq:
    - id: modules_entries
      type: modules_entries
      repeat: eos
  modules_0:
    seq:
    - id: len_modules
      type: u4
      valid:
        max: 1073741823
    - id: modules
      type: modules
      size: len_modules
  modules_entries:
    seq:
    - id: modules_elt
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
- id: hash_tag
  type: u1
  enum: bool
- id: hash
  size: 32
  if: (hash_tag == bool::true)
  doc: Used to force the hash of the protocol
- id: expected_env_version_tag
  type: u1
  enum: bool
- id: expected_env_version
  type: protocol__environment_version
  if: (expected_env_version_tag == bool::true)
- id: modules
  type: modules_0
  doc: Modules comprising the protocol
