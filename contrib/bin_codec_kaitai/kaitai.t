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
