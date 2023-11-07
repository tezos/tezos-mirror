meta:
  id: id_014__ptkathma__operation__internal
  endian: be
types:
  id_014__ptkathma__apply_internal_results__alpha__operation_result:
    seq:
    - id: source
      type: id_014__ptkathma__contract_id
    - id: nonce
      type: u2
    - id: id_014__ptkathma__apply_internal_results__alpha__operation_result_tag
      type: u1
      enum: id_014__ptkathma__apply_internal_results__alpha__operation_result_tag
    - id: id_014__ptkathma__apply_internal_results__alpha__operation_result_transaction
      type: id_014__ptkathma__apply_internal_results__alpha__operation_result_transaction
      if: (id_014__ptkathma__apply_internal_results__alpha__operation_result_tag ==
        id_014__ptkathma__apply_internal_results__alpha__operation_result_tag::transaction)
    - id: id_014__ptkathma__apply_internal_results__alpha__operation_result_origination
      type: id_014__ptkathma__apply_internal_results__alpha__operation_result_origination
      if: (id_014__ptkathma__apply_internal_results__alpha__operation_result_tag ==
        id_014__ptkathma__apply_internal_results__alpha__operation_result_tag::origination)
    - id: id_014__ptkathma__apply_internal_results__alpha__operation_result_delegation
      type: id_014__ptkathma__apply_internal_results__alpha__operation_result_delegation
      if: (id_014__ptkathma__apply_internal_results__alpha__operation_result_tag ==
        id_014__ptkathma__apply_internal_results__alpha__operation_result_tag::delegation)
    - id: id_014__ptkathma__apply_internal_results__alpha__operation_result_event
      type: id_014__ptkathma__apply_internal_results__alpha__operation_result_event
      if: (id_014__ptkathma__apply_internal_results__alpha__operation_result_tag ==
        id_014__ptkathma__apply_internal_results__alpha__operation_result_tag::event)
  id_014__ptkathma__apply_internal_results__alpha__operation_result_event:
    seq:
    - id: type
      type: micheline__014__ptkathma__michelson_v1__expression
    - id: tag_tag
      type: u1
      enum: bool
    - id: tag
      type: id_014__ptkathma__entrypoint
      if: (tag_tag == bool::true)
    - id: payload_tag
      type: u1
      enum: bool
    - id: payload
      type: micheline__014__ptkathma__michelson_v1__expression
      if: (payload_tag == bool::true)
  micheline__014__ptkathma__michelson_v1__expression:
    seq:
    - id: micheline__014__ptkathma__michelson_v1__expression_tag
      type: u1
      enum: micheline__014__ptkathma__michelson_v1__expression_tag
    - id: micheline__014__ptkathma__michelson_v1__expression_int
      type: z
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::int)
    - id: micheline__014__ptkathma__michelson_v1__expression_string
      type: string
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::string)
    - id: micheline__014__ptkathma__michelson_v1__expression_sequence
      type: micheline__014__ptkathma__michelson_v1__expression_sequence
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::sequence)
    - id: micheline__014__ptkathma__michelson_v1__expression_prim__no_args__no_annots
      type: u1
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::prim__no_args__no_annots)
      enum: id_014__ptkathma__michelson__v1__primitives
    - id: micheline__014__ptkathma__michelson_v1__expression_prim__no_args__some_annots
      type: micheline__014__ptkathma__michelson_v1__expression_prim__no_args__some_annots
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::prim__no_args__some_annots)
    - id: micheline__014__ptkathma__michelson_v1__expression_prim__1_arg__no_annots
      type: micheline__014__ptkathma__michelson_v1__expression_prim__1_arg__no_annots
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::prim__1_arg__no_annots)
    - id: micheline__014__ptkathma__michelson_v1__expression_prim__1_arg__some_annots
      type: micheline__014__ptkathma__michelson_v1__expression_prim__1_arg__some_annots
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::prim__1_arg__some_annots)
    - id: micheline__014__ptkathma__michelson_v1__expression_prim__2_args__no_annots
      type: micheline__014__ptkathma__michelson_v1__expression_prim__2_args__no_annots
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::prim__2_args__no_annots)
    - id: micheline__014__ptkathma__michelson_v1__expression_prim__2_args__some_annots
      type: micheline__014__ptkathma__michelson_v1__expression_prim__2_args__some_annots
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::prim__2_args__some_annots)
    - id: micheline__014__ptkathma__michelson_v1__expression_prim__generic
      type: micheline__014__ptkathma__michelson_v1__expression_prim__generic
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::prim__generic)
    - id: micheline__014__ptkathma__michelson_v1__expression_bytes
      type: bytes
      if: (micheline__014__ptkathma__michelson_v1__expression_tag == micheline__014__ptkathma__michelson_v1__expression_tag::bytes)
  bytes:
    seq:
    - id: len_bytes
      type: s4
    - id: bytes
      size: len_bytes
  micheline__014__ptkathma__michelson_v1__expression_prim__generic:
    seq:
    - id: prim
      type: u1
      enum: id_014__ptkathma__michelson__v1__primitives
    - id: args
      type: args
    - id: annots
      type: annots
  args:
    seq:
    - id: len_args
      type: s4
    - id: args
      type: args_entries
      size: len_args
      repeat: eos
  args_entries:
    seq:
    - id: args_elt
      type: micheline__014__ptkathma__michelson_v1__expression
  micheline__014__ptkathma__michelson_v1__expression_prim__2_args__some_annots:
    seq:
    - id: prim
      type: u1
      enum: id_014__ptkathma__michelson__v1__primitives
    - id: arg1
      type: micheline__014__ptkathma__michelson_v1__expression
    - id: arg2
      type: micheline__014__ptkathma__michelson_v1__expression
    - id: annots
      type: annots
  micheline__014__ptkathma__michelson_v1__expression_prim__2_args__no_annots:
    seq:
    - id: prim
      type: u1
      enum: id_014__ptkathma__michelson__v1__primitives
    - id: arg1
      type: micheline__014__ptkathma__michelson_v1__expression
    - id: arg2
      type: micheline__014__ptkathma__michelson_v1__expression
  micheline__014__ptkathma__michelson_v1__expression_prim__1_arg__some_annots:
    seq:
    - id: prim
      type: u1
      enum: id_014__ptkathma__michelson__v1__primitives
    - id: arg
      type: micheline__014__ptkathma__michelson_v1__expression
    - id: annots
      type: annots
  micheline__014__ptkathma__michelson_v1__expression_prim__1_arg__no_annots:
    seq:
    - id: prim
      type: u1
      enum: id_014__ptkathma__michelson__v1__primitives
    - id: arg
      type: micheline__014__ptkathma__michelson_v1__expression
  micheline__014__ptkathma__michelson_v1__expression_prim__no_args__some_annots:
    seq:
    - id: prim
      type: u1
      enum: id_014__ptkathma__michelson__v1__primitives
    - id: annots
      type: annots
  annots:
    seq:
    - id: len_annots
      type: s4
    - id: annots
      size: len_annots
  micheline__014__ptkathma__michelson_v1__expression_sequence:
    seq:
    - id: len_sequence
      type: s4
    - id: sequence
      type: sequence_entries
      size: len_sequence
      repeat: eos
  sequence_entries:
    seq:
    - id: sequence_elt
      type: micheline__014__ptkathma__michelson_v1__expression
  string:
    seq:
    - id: len_string
      type: s4
    - id: string
      size: len_string
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
  id_014__ptkathma__apply_internal_results__alpha__operation_result_delegation:
    seq:
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
  id_014__ptkathma__apply_internal_results__alpha__operation_result_origination:
    seq:
    - id: balance
      type: n
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
    - id: script
      type: id_014__ptkathma__scripted__contracts
  id_014__ptkathma__scripted__contracts:
    seq:
    - id: code
      type: code
    - id: storage
      type: storage
  storage:
    seq:
    - id: len_storage
      type: s4
    - id: storage
      size: len_storage
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  id_014__ptkathma__apply_internal_results__alpha__operation_result_transaction:
    seq:
    - id: amount
      type: n
    - id: destination
      type: id_014__ptkathma__transaction_destination
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
  parameters:
    seq:
    - id: entrypoint
      type: id_014__ptkathma__entrypoint
    - id: value
      type: value
  value:
    seq:
    - id: len_value
      type: s4
    - id: value
      size: len_value
  id_014__ptkathma__entrypoint:
    doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    seq:
    - id: id_014__ptkathma__entrypoint_tag
      type: u1
      enum: id_014__ptkathma__entrypoint_tag
    - id: id_014__ptkathma__entrypoint_named
      type: id_014__ptkathma__entrypoint_named
      if: (id_014__ptkathma__entrypoint_tag == id_014__ptkathma__entrypoint_tag::named)
  id_014__ptkathma__entrypoint_named:
    seq:
    - id: len_named
      type: u1
    - id: named
      size: len_named
      size-eos: true
  id_014__ptkathma__transaction_destination:
    doc: ! >-
      A destination of a transaction: A destination notation compatible with the contract
      notation as given to an RPC or inside scripts. Can be a base58 implicit contract
      hash, a base58 originated contract hash, a base58 originated transaction rollup,
      or a base58 originated smart-contract rollup.
    seq:
    - id: id_014__ptkathma__transaction_destination_tag
      type: u1
      enum: id_014__ptkathma__transaction_destination_tag
    - id: id_014__ptkathma__transaction_destination_implicit
      type: public_key_hash
      if: (id_014__ptkathma__transaction_destination_tag == id_014__ptkathma__transaction_destination_tag::implicit)
    - id: id_014__ptkathma__transaction_destination_originated
      type: id_014__ptkathma__transaction_destination_originated
      if: (id_014__ptkathma__transaction_destination_tag == id_014__ptkathma__transaction_destination_tag::originated)
    - id: id_014__ptkathma__transaction_destination_tx_rollup
      type: id_014__ptkathma__transaction_destination_tx_rollup
      if: (id_014__ptkathma__transaction_destination_tag == id_014__ptkathma__transaction_destination_tag::tx_rollup)
    - id: id_014__ptkathma__transaction_destination_sc_rollup
      type: id_014__ptkathma__transaction_destination_sc_rollup
      if: (id_014__ptkathma__transaction_destination_tag == id_014__ptkathma__transaction_destination_tag::sc_rollup)
  id_014__ptkathma__transaction_destination_sc_rollup:
    seq:
    - id: sc_rollup_hash
      size: 20
    - id: sc_rollup_padding
      size: 1
      doc: This field is for padding, ignore
  id_014__ptkathma__transaction_destination_tx_rollup:
    seq:
    - id: rollup_hash
      size: 20
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: tx_rollup_padding
      size: 1
      doc: This field is for padding, ignore
  id_014__ptkathma__transaction_destination_originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
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
  id_014__ptkathma__contract_id:
    doc: ! >-
      A contract handle: A contract notation as given to an RPC or inside scripts.
      Can be a base58 implicit contract hash or a base58 originated contract hash.
    seq:
    - id: id_014__ptkathma__contract_id_tag
      type: u1
      enum: id_014__ptkathma__contract_id_tag
    - id: id_014__ptkathma__contract_id_implicit
      type: public_key_hash
      if: (id_014__ptkathma__contract_id_tag == id_014__ptkathma__contract_id_tag::implicit)
    - id: id_014__ptkathma__contract_id_originated
      type: id_014__ptkathma__contract_id_originated
      if: (id_014__ptkathma__contract_id_tag == id_014__ptkathma__contract_id_tag::originated)
  id_014__ptkathma__contract_id_originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  public_key_hash:
    doc: A Ed25519, Secp256k1, or P256 public key hash
    seq:
    - id: public_key_hash_tag
      type: u1
      enum: public_key_hash_tag
    - id: public_key_hash_ed25519
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::ed25519)
    - id: public_key_hash_secp256k1
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::secp256k1)
    - id: public_key_hash_p256
      size: 20
      if: (public_key_hash_tag == public_key_hash_tag::p256)
enums:
  id_014__ptkathma__michelson__v1__primitives:
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
      id: none_0
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
      id: none
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
      id: ticket_0
      doc: TICKET
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
  micheline__014__ptkathma__michelson_v1__expression_tag:
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
  id_014__ptkathma__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    255: named
  bool:
    0: false
    255: true
  id_014__ptkathma__transaction_destination_tag:
    0: implicit
    1: originated
    2: tx_rollup
    3: sc_rollup
  id_014__ptkathma__apply_internal_results__alpha__operation_result_tag:
    1: transaction
    2: origination
    3: delegation
    4: event
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  id_014__ptkathma__contract_id_tag:
    0: implicit
    1: originated
seq:
- id: id_014__ptkathma__apply_internal_results__alpha__operation_result
  type: id_014__ptkathma__apply_internal_results__alpha__operation_result
