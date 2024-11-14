meta:
  id: id_021__psquebec__smart_rollup__inbox__message
  endian: be
  imports:
  - timestamp__protocol
doc: ! 'Encoding id: 021-PsQuebec.smart_rollup.inbox.message'
types:
  args:
    seq:
    - id: args_entries
      type: args_entries
      repeat: eos
  args_0:
    seq:
    - id: len_args
      type: u4be
      valid:
        max: 1073741823
    - id: args
      type: args
      size: len_args
  args_entries:
    seq:
    - id: args_elt
      type: micheline__021__psquebec__michelson_v1__expression
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  id_021__psquebec__michelson__v1__primitives:
    seq:
    - id: id_021__psquebec__michelson__v1__primitives
      type: u1
      enum: id_021__psquebec__michelson__v1__primitives
  info_per_level:
    seq:
    - id: predecessor_timestamp
      type: timestamp__protocol
    - id: predecessor
      size: 32
  internal:
    seq:
    - id: internal_tag
      type: u1
      enum: internal_tag
    - id: transfer
      type: transfer
      if: (internal_tag == internal_tag::transfer)
    - id: info_per_level
      type: info_per_level
      if: (internal_tag == internal_tag::info_per_level)
    - id: protocol_migration
      type: bytes_dyn_uint30
      if: (internal_tag == internal_tag::protocol_migration)
  micheline__021__psquebec__michelson_v1__expression:
    seq:
    - id: micheline__021__psquebec__michelson_v1__expression_tag
      type: u1
      enum: micheline__021__psquebec__michelson_v1__expression_tag
    - id: int
      type: z
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::int)
    - id: string
      type: bytes_dyn_uint30
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::string)
    - id: sequence
      type: sequence_0
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::sequence)
    - id: prim__no_args__no_annots
      type: id_021__psquebec__michelson__v1__primitives
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::prim__no_args__no_annots)
    - id: prim__no_args__some_annots
      type: prim__no_args__some_annots
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::prim__no_args__some_annots)
    - id: prim__1_arg__no_annots
      type: prim__1_arg__no_annots
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::prim__1_arg__no_annots)
    - id: prim__1_arg__some_annots
      type: prim__1_arg__some_annots
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::prim__1_arg__some_annots)
    - id: prim__2_args__no_annots
      type: prim__2_args__no_annots
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::prim__2_args__no_annots)
    - id: prim__2_args__some_annots
      type: prim__2_args__some_annots
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::prim__2_args__some_annots)
    - id: prim__generic
      type: prim__generic
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::prim__generic)
    - id: bytes
      type: bytes_dyn_uint30
      if: (micheline__021__psquebec__michelson_v1__expression_tag == micheline__021__psquebec__michelson_v1__expression_tag::bytes)
  n_chunk:
    seq:
    - id: has_more
      type: b1be
    - id: payload
      type: b7be
  prim__1_arg__no_annots:
    seq:
    - id: prim
      type: id_021__psquebec__michelson__v1__primitives
    - id: arg
      type: micheline__021__psquebec__michelson_v1__expression
  prim__1_arg__some_annots:
    seq:
    - id: prim
      type: id_021__psquebec__michelson__v1__primitives
    - id: arg
      type: micheline__021__psquebec__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__2_args__no_annots:
    seq:
    - id: prim
      type: id_021__psquebec__michelson__v1__primitives
    - id: arg1
      type: micheline__021__psquebec__michelson_v1__expression
    - id: arg2
      type: micheline__021__psquebec__michelson_v1__expression
  prim__2_args__some_annots:
    seq:
    - id: prim
      type: id_021__psquebec__michelson__v1__primitives
    - id: arg1
      type: micheline__021__psquebec__michelson_v1__expression
    - id: arg2
      type: micheline__021__psquebec__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__generic:
    seq:
    - id: prim
      type: id_021__psquebec__michelson__v1__primitives
    - id: args
      type: args_0
    - id: annots
      type: bytes_dyn_uint30
  prim__no_args__some_annots:
    seq:
    - id: prim
      type: id_021__psquebec__michelson__v1__primitives
    - id: annots
      type: bytes_dyn_uint30
  public_key_hash:
    seq:
    - id: public_key_hash_tag
      type: u1
      enum: public_key_hash_tag
    - id: ed25519
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::ed25519)
    - id: secp256k1
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::secp256k1)
    - id: p256
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::p256)
    - id: bls
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::bls)
  sequence:
    seq:
    - id: sequence_entries
      type: sequence_entries
      repeat: eos
  sequence_0:
    seq:
    - id: len_sequence
      type: u4be
      valid:
        max: 1073741823
    - id: sequence
      type: sequence
      size: len_sequence
  sequence_entries:
    seq:
    - id: sequence_elt
      type: micheline__021__psquebec__michelson_v1__expression
  transfer:
    seq:
    - id: payload
      type: micheline__021__psquebec__michelson_v1__expression
    - id: sender
      size: 20
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: destination
      size: 20
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
enums:
  id_021__psquebec__michelson__v1__primitives:
    0: parameter
    1: storage
    2: code
    3:
      id: false
      doc: False
    4:
      id: elt
      doc: Elt
    5:
      id: left
      doc: Left
    6:
      id: none
      doc: None
    7:
      id: pair_1
      doc: Pair
    8:
      id: right
      doc: Right
    9:
      id: some_0
      doc: Some
    10:
      id: true
      doc: True
    11:
      id: unit_1
      doc: Unit
    12:
      id: pack
      doc: PACK
    13:
      id: unpack
      doc: UNPACK
    14:
      id: blake2b
      doc: BLAKE2B
    15:
      id: sha256
      doc: SHA256
    16:
      id: sha512
      doc: SHA512
    17:
      id: abs
      doc: ABS
    18:
      id: add
      doc: ADD
    19:
      id: amount
      doc: AMOUNT
    20:
      id: and
      doc: AND
    21:
      id: balance
      doc: BALANCE
    22:
      id: car
      doc: CAR
    23:
      id: cdr
      doc: CDR
    24:
      id: check_signature
      doc: CHECK_SIGNATURE
    25:
      id: compare
      doc: COMPARE
    26:
      id: concat
      doc: CONCAT
    27:
      id: cons
      doc: CONS
    28:
      id: create_account
      doc: CREATE_ACCOUNT
    29:
      id: create_contract
      doc: CREATE_CONTRACT
    30:
      id: implicit_account
      doc: IMPLICIT_ACCOUNT
    31:
      id: dip
      doc: DIP
    32:
      id: drop
      doc: DROP
    33:
      id: dup
      doc: DUP
    34:
      id: ediv
      doc: EDIV
    35:
      id: empty_map
      doc: EMPTY_MAP
    36:
      id: empty_set
      doc: EMPTY_SET
    37:
      id: eq
      doc: EQ
    38:
      id: exec
      doc: EXEC
    39:
      id: failwith
      doc: FAILWITH
    40:
      id: ge
      doc: GE
    41:
      id: get
      doc: GET
    42:
      id: gt
      doc: GT
    43:
      id: hash_key
      doc: HASH_KEY
    44:
      id: if
      doc: IF
    45:
      id: if_cons
      doc: IF_CONS
    46:
      id: if_left
      doc: IF_LEFT
    47:
      id: if_none
      doc: IF_NONE
    48:
      id: int_0
      doc: INT
    49:
      id: lambda_0
      doc: LAMBDA
    50:
      id: le
      doc: LE
    51:
      id: left_0
      doc: LEFT
    52:
      id: loop
      doc: LOOP
    53:
      id: lsl
      doc: LSL
    54:
      id: lsr
      doc: LSR
    55:
      id: lt
      doc: LT
    56:
      id: map_0
      doc: MAP
    57:
      id: mem
      doc: MEM
    58:
      id: mul
      doc: MUL
    59:
      id: neg
      doc: NEG
    60:
      id: neq
      doc: NEQ
    61:
      id: nil
      doc: NIL
    62:
      id: none_0
      doc: NONE
    63:
      id: not
      doc: NOT
    64:
      id: now
      doc: NOW
    65:
      id: or_0
      doc: OR
    66:
      id: pair_0
      doc: PAIR
    67:
      id: push
      doc: PUSH
    68:
      id: right_0
      doc: RIGHT
    69:
      id: size
      doc: SIZE
    70:
      id: some
      doc: SOME
    71:
      id: source
      doc: SOURCE
    72:
      id: sender
      doc: SENDER
    73:
      id: self
      doc: SELF
    74:
      id: steps_to_quota
      doc: STEPS_TO_QUOTA
    75:
      id: sub
      doc: SUB
    76:
      id: swap
      doc: SWAP
    77:
      id: transfer_tokens
      doc: TRANSFER_TOKENS
    78:
      id: set_delegate
      doc: SET_DELEGATE
    79:
      id: unit_0
      doc: UNIT
    80:
      id: update
      doc: UPDATE
    81:
      id: xor
      doc: XOR
    82:
      id: iter
      doc: ITER
    83:
      id: loop_left
      doc: LOOP_LEFT
    84:
      id: address_0
      doc: ADDRESS
    85:
      id: contract_0
      doc: CONTRACT
    86:
      id: isnat
      doc: ISNAT
    87:
      id: cast
      doc: CAST
    88:
      id: rename
      doc: RENAME
    89: bool
    90: contract
    91: int
    92: key
    93: key_hash
    94: lambda
    95: list
    96: map
    97: big_map
    98: nat
    99: option
    100: or
    101: pair
    102: set
    103: signature
    104: string
    105: bytes
    106: mutez
    107: timestamp
    108: unit
    109: operation
    110: address
    111:
      id: slice
      doc: SLICE
    112:
      id: dig
      doc: DIG
    113:
      id: dug
      doc: DUG
    114:
      id: empty_big_map
      doc: EMPTY_BIG_MAP
    115:
      id: apply
      doc: APPLY
    116: chain_id
    117:
      id: chain_id_0
      doc: CHAIN_ID
    118:
      id: level
      doc: LEVEL
    119:
      id: self_address
      doc: SELF_ADDRESS
    120: never
    121:
      id: never_0
      doc: NEVER
    122:
      id: unpair
      doc: UNPAIR
    123:
      id: voting_power
      doc: VOTING_POWER
    124:
      id: total_voting_power
      doc: TOTAL_VOTING_POWER
    125:
      id: keccak
      doc: KECCAK
    126:
      id: sha3
      doc: SHA3
    127:
      id: pairing_check
      doc: PAIRING_CHECK
    128: bls12_381_g1
    129: bls12_381_g2
    130: bls12_381_fr
    131: sapling_state
    132: sapling_transaction_deprecated
    133:
      id: sapling_empty_state
      doc: SAPLING_EMPTY_STATE
    134:
      id: sapling_verify_update
      doc: SAPLING_VERIFY_UPDATE
    135: ticket
    136:
      id: ticket_deprecated
      doc: TICKET_DEPRECATED
    137:
      id: read_ticket
      doc: READ_TICKET
    138:
      id: split_ticket
      doc: SPLIT_TICKET
    139:
      id: join_tickets
      doc: JOIN_TICKETS
    140:
      id: get_and_update
      doc: GET_AND_UPDATE
    141: chest
    142: chest_key
    143:
      id: open_chest
      doc: OPEN_CHEST
    144:
      id: view_0
      doc: VIEW
    145: view
    146: constant
    147:
      id: sub_mutez
      doc: SUB_MUTEZ
    148: tx_rollup_l2_address
    149:
      id: min_block_time
      doc: MIN_BLOCK_TIME
    150: sapling_transaction
    151:
      id: emit
      doc: EMIT
    152:
      id: lambda_rec_0
      doc: Lambda_rec
    153:
      id: lambda_rec
      doc: LAMBDA_REC
    154:
      id: ticket_0
      doc: TICKET
    155:
      id: bytes_0
      doc: BYTES
    156:
      id: nat_0
      doc: NAT
    157:
      id: ticket_1
      doc: Ticket
  id_021__psquebec__smart_rollup__inbox__message_tag:
    0: internal
    1: external
  internal_tag:
    0: transfer
    1: start_of_level
    2: end_of_level
    3: info_per_level
    4: protocol_migration
  micheline__021__psquebec__michelson_v1__expression_tag:
    0: int
    1: string
    2: sequence
    3:
      id: prim__no_args__no_annots
      doc: Primitive with no arguments and no annotations
    4:
      id: prim__no_args__some_annots
      doc: Primitive with no arguments and some annotations
    5:
      id: prim__1_arg__no_annots
      doc: Primitive with one argument and no annotations
    6:
      id: prim__1_arg__some_annots
      doc: Primitive with one argument and some annotations
    7:
      id: prim__2_args__no_annots
      doc: Primitive with two arguments and no annotations
    8:
      id: prim__2_args__some_annots
      doc: Primitive with two arguments and some annotations
    9:
      id: prim__generic
      doc: Generic primitive (any number of args with or without annotations)
    10: bytes
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
seq:
- id: id_021__psquebec__smart_rollup__inbox__message_tag
  type: u1
  enum: id_021__psquebec__smart_rollup__inbox__message_tag
- id: internal
  type: internal
  if: (id_021__psquebec__smart_rollup__inbox__message_tag == id_021__psquebec__smart_rollup__inbox__message_tag::internal)
- id: external
  size-eos: true
  if: (id_021__psquebec__smart_rollup__inbox__message_tag == id_021__psquebec__smart_rollup__inbox__message_tag::external)
