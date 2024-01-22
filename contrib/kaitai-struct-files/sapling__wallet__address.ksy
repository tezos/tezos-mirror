meta:
  id: sapling__wallet__address
  endian: be
doc: ! 'Encoding id: sapling.wallet.address'
types:
  sapling__wallet__diversifier:
    seq:
    - id: sapling__wallet__diversifier
      size: 11
seq:
- id: diversifier
  type: sapling__wallet__diversifier
- id: pkd
  size: 32
