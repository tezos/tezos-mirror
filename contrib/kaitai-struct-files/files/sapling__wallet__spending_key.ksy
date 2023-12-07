meta:
  id: sapling__wallet__spending_key
  endian: be
doc: ! 'Encoding id: sapling.wallet.spending_key'
types:
  sapling__wallet__expanded_spending_key:
    seq:
    - id: ask
      size: 32
    - id: nsk
      size: 32
    - id: ovk
      size: 32
seq:
- id: depth
  size: 1
- id: parent_fvk_tag
  size: 4
- id: child_index
  size: 4
- id: chain_code
  size: 32
- id: expsk
  type: sapling__wallet__expanded_spending_key
- id: dk
  size: 32
