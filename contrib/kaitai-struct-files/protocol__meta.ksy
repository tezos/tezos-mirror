meta:
  id: protocol__meta
  endian: be
doc: ! >-
  Encoding id: protocol.meta

  Description: Protocol metadata: the hash of the protocol, the expected environment
  version and the list of modules comprising the protocol.
types:
  modules:
    seq:
    - id: len_modules
      type: s4
    - id: modules
      type: modules_entries
      size: len_modules
      repeat: eos
  modules_entries:
    seq:
    - id: len_modules_elt
      type: s4
    - id: modules_elt
      size: len_modules_elt
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
  type: u2
  if: (expected_env_version_tag == bool::true)
- id: modules
  type: modules
  doc: Modules comprising the protocol
