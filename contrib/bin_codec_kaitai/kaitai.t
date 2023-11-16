ground.uint8 test
  $ ./codec.exe dump kaitai for ground.uint8
  meta:
    id: ground__uint8
    endian: be
  doc: ! 'Encoding id: ground.uint8
  
    Description: Unsigned 8 bit integers'
  seq:
  - id: ground__uint8
    type: u1
ground.bool test
  $ ./codec.exe dump kaitai for ground.bool
  meta:
    id: ground__bool
    endian: be
  doc: ! 'Encoding id: ground.bool
  
    Description: Boolean values'
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
  doc: ! 'Encoding id: ground.int8
  
    Description: Signed 8 bit integers'
  seq:
  - id: ground__int8
    type: s1
ground.uint16 test
  $ ./codec.exe dump kaitai for ground.uint16
  meta:
    id: ground__uint16
    endian: be
  doc: ! 'Encoding id: ground.uint16
  
    Description: Unsigned 16 bit integers'
  seq:
  - id: ground__uint16
    type: u2
ground.int16 test
  $ ./codec.exe dump kaitai for ground.int16
  meta:
    id: ground__int16
    endian: be
  doc: ! 'Encoding id: ground.int16
  
    Description: Signed 16 bit integers'
  seq:
  - id: ground__int16
    type: s2
ground.int32 test
  $ ./codec.exe dump kaitai for ground.int32
  meta:
    id: ground__int32
    endian: be
  doc: ! 'Encoding id: ground.int32
  
    Description: Signed 32 bit integers'
  seq:
  - id: ground__int32
    type: s4
ground.int64 test
  $ ./codec.exe dump kaitai for ground.int64
  meta:
    id: ground__int64
    endian: be
  doc: ! 'Encoding id: ground.int64
  
    Description: Signed 64 bit integers'
  seq:
  - id: ground__int64
    type: s8
ground.int31 test
  $ ./codec.exe dump kaitai for ground.int31
  meta:
    id: ground__int31
    endian: be
  doc: ! 'Encoding id: ground.int31
  
    Description: Signed 31 bit integers'
  types:
    int31:
      seq:
      - id: int31
        type: s4
        valid:
          min: -1073741824
          max: 1073741823
  seq:
  - id: ground__int31
    type: int31
ground.float test
  $ ./codec.exe dump kaitai for ground.float
  meta:
    id: ground__float
    endian: be
  doc: ! 'Encoding id: ground.float
  
    Description: Floating point numbers'
  seq:
  - id: ground__float
    type: f8
ground.bytes test
  $ ./codec.exe dump kaitai for ground.bytes
  meta:
    id: ground__bytes
    endian: be
  doc: ! 'Encoding id: ground.bytes'
  types:
    bytes_dyn_uint30:
      seq:
      - id: len_bytes_dyn_uint30
        type: u4
        valid:
          max: 1073741823
      - id: bytes_dyn_uint30
        size: len_bytes_dyn_uint30
  seq:
  - id: ground__bytes
    type: bytes_dyn_uint30
ground.string test
  $ ./codec.exe dump kaitai for ground.string
  meta:
    id: ground__string
    endian: be
  doc: ! 'Encoding id: ground.string'
  types:
    bytes_dyn_uint30:
      seq:
      - id: len_bytes_dyn_uint30
        type: u4
        valid:
          max: 1073741823
      - id: bytes_dyn_uint30
        size: len_bytes_dyn_uint30
  seq:
  - id: ground__string
    type: bytes_dyn_uint30
ground.N test
  $ ./codec.exe dump kaitai for ground.N
  meta:
    id: ground__n
    endian: be
  doc: ! 'Encoding id: ground.N
  
    Description: Arbitrary precision natural numbers'
  types:
    n:
      seq:
      - id: n
        type: n_chunk
        repeat: until
        repeat-until: not (_.has_more).as<bool>
    n_chunk:
      seq:
      - id: has_more
        type: b1be
      - id: payload
        type: b7be
  seq:
  - id: ground__n
    type: n
ground.Z test
  $ ./codec.exe dump kaitai for ground.Z
  meta:
    id: ground__z
    endian: be
  doc: ! 'Encoding id: ground.Z
  
    Description: Arbitrary precision integers'
  types:
    n_chunk:
      seq:
      - id: has_more
        type: b1be
      - id: payload
        type: b7be
    z:
      seq:
      - id: has_tail
        type: b1be
      - id: sign
        type: b1be
      - id: payload
        type: b6be
      - id: tail
        type: n_chunk
        repeat: until
        repeat-until: not (_.has_more).as<bool>
        if: has_tail.as<bool>
  seq:
  - id: ground__z
    type: z
