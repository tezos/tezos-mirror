ground.uint8 test
  $ ./codec.exe dump kaitai for ground.uint8
  meta:
    id: ground__uint8
    endian: be
  doc: Unsigned 8 bit integers
  seq:
  - id: ground__uint8
    type: u1
ground.bool test
  $ ./codec.exe dump kaitai for ground.bool
  meta:
    id: ground__bool
    endian: be
  doc: Boolean values
  enums:
    bool:
      0: false
      255: true
  seq:
  - id: ground__bool
    type: u1
    enum: bool
ground.int8 test
  $ ./codec.exe dump kaitai for ground.int8
  meta:
    id: ground__int8
    endian: be
  doc: Signed 8 bit integers
  seq:
  - id: ground__int8
    type: s1
ground.uint16 test
  $ ./codec.exe dump kaitai for ground.uint16
  meta:
    id: ground__uint16
    endian: be
  doc: Unsigned 16 bit integers
  seq:
  - id: ground__uint16
    type: u2
ground.int16 test
  $ ./codec.exe dump kaitai for ground.int16
  meta:
    id: ground__int16
    endian: be
  doc: Signed 16 bit integers
  seq:
  - id: ground__int16
    type: s2
ground.int32 test
  $ ./codec.exe dump kaitai for ground.int32
  meta:
    id: ground__int32
    endian: be
  doc: Signed 32 bit integers
  seq:
  - id: ground__int32
    type: s4
ground.int64 test
  $ ./codec.exe dump kaitai for ground.int64
  meta:
    id: ground__int64
    endian: be
  doc: Signed 64 bit integers
  seq:
  - id: ground__int64
    type: s8
ground.int31 test
  $ ./codec.exe dump kaitai for ground.int31
  meta:
    id: ground__int31
    endian: be
  doc: Signed 31 bit integers
  seq:
  - id: ground__int31
    type: s4
ground.float test
  $ ./codec.exe dump kaitai for ground.float
  meta:
    id: ground__float
    endian: be
  doc: Floating point numbers
  seq:
  - id: ground__float
    type: f8
ground.bytes test
  $ ./codec.exe dump kaitai for ground.bytes
  meta:
    id: ground__bytes
    endian: be
  types:
    fixed_bytes:
      seq:
      - id: size
        type: u4
      - id: value
        size: size
  seq:
  - id: ground__bytes
    type: fixed_bytes
ground.string test
  $ ./codec.exe dump kaitai for ground.string
  meta:
    id: ground__string
    endian: be
  types:
    fixed_bytes:
      seq:
      - id: size
        type: u4
      - id: value
        size: size
  seq:
  - id: ground__string
    type: fixed_bytes
ground.N test
  $ ./codec.exe dump kaitai for ground.N
  meta:
    id: ground__n
    endian: be
  doc: Arbitrary precision natural numbers
  types:
    group:
      instances:
        has_next:
          value: ((b & 128) != 0)
        value:
          value: (b & 127)
      seq:
      - id: b
        type: u1
  seq:
  - id: groups
    type: group
    repeat: until
    repeat-until: not (_.has_next)
ground.Z test
  $ ./codec.exe dump kaitai for ground.Z
  meta:
    id: ground__z
    endian: be
  doc: Arbitrary precision integers
  types:
    group:
      instances:
        has_next:
          value: ((b & 128) != 0)
        value:
          value: (b & 127)
      seq:
      - id: b
        type: u1
  instances:
    is_negative:
      value: (((groups[0].value) >> 6) == 1)
  seq:
  - id: groups
    type: group
    repeat: until
    repeat-until: not (_.has_next)
