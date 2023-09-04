ground.uint8 test
  $ ./codec.exe dump kaitai for ground.uint8
  meta:
    id: ground__uint8
    endian: be
  seq:
  - id: uint8
    type: u1
ground.bool test
  $ ./codec.exe dump kaitai for ground.bool
  meta:
    id: ground__bool
    endian: be
  enums:
    bool:
      0: false
      255: true
  seq:
  - id: bool
    type: u1
    enum: bool
ground.int8 test
  $ ./codec.exe dump kaitai for ground.int8
  meta:
    id: ground__int8
    endian: be
  seq:
  - id: int8
    type: s1
ground.uint16 test
  $ ./codec.exe dump kaitai for ground.uint16
  meta:
    id: ground__uint16
    endian: be
  seq:
  - id: uint16
    type: u2
ground.int16 test
  $ ./codec.exe dump kaitai for ground.int16
  meta:
    id: ground__int16
    endian: be
  seq:
  - id: int16
    type: s2
ground.int32 test
  $ ./codec.exe dump kaitai for ground.int32
  meta:
    id: ground__int32
    endian: be
  seq:
  - id: int32
    type: s4
ground.int64 test
  $ ./codec.exe dump kaitai for ground.int64
  meta:
    id: ground__int64
    endian: be
  seq:
  - id: int64
    type: s8
ground.int31 test
  $ ./codec.exe dump kaitai for ground.int31
  meta:
    id: ground__int31
    endian: be
  seq:
  - id: int31
    type: s4
ground.float test
  $ ./codec.exe dump kaitai for ground.float
  meta:
    id: ground__float
    endian: be
  seq:
  - id: float
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
  - id: fixed size (uint30) bytes
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
  - id: fixed size (uint30) bytes
    type: fixed_bytes
