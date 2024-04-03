meta:
  id: id_013__ptjakart__operation__unsigned
  endian: be
  imports:
  - block_header__shell
  - operation__shell_header
doc: ! 'Encoding id: 013-PtJakart.operation.unsigned'
types:
  activate_account:
    seq:
    - id: pkh
      size: 20
    - id: secret
      size: 20
  amount:
    seq:
    - id: amount_tag
      type: u1
      enum: amount_tag
    - id: small
      type: u1
      if: (amount_tag == amount_tag::small)
    - id: medium
      type: u2be
      if: (amount_tag == amount_tag::medium)
    - id: biggish
      type: s4be
      if: (amount_tag == amount_tag::biggish)
    - id: bigger
      type: s8be
      if: (amount_tag == amount_tag::bigger)
  ballot:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: period
      type: s4be
    - id: proposal
      size: 32
    - id: ballot
      type: s1
  bh1:
    seq:
    - id: id_013__ptjakart__block_header__alpha__full_header
      type: id_013__ptjakart__block_header__alpha__full_header
  bh1_0:
    seq:
    - id: len_bh1
      type: u4be
      valid:
        max: 1073741823
    - id: bh1
      type: bh1
      size: len_bh1
  bh2:
    seq:
    - id: id_013__ptjakart__block_header__alpha__full_header
      type: id_013__ptjakart__block_header__alpha__full_header
  bh2_0:
    seq:
    - id: len_bh2
      type: u4be
      valid:
        max: 1073741823
    - id: bh2
      type: bh2
      size: len_bh2
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4be
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  case_0:
    seq:
    - id: case_0_field0
      type: s2be
    - id: case_0_field1
      size: 32
      doc: context_hash
    - id: case_0_field2
      size: 32
      doc: context_hash
    - id: case_0_field3
      type: case_0_field3_0
  case_0_field3:
    seq:
    - id: case_0_field3_entries
      type: case_0_field3_entries
      repeat: eos
  case_0_field3_0:
    seq:
    - id: len_case_0_field3
      type: u4be
      valid:
        max: 1073741823
    - id: case_0_field3
      type: case_0_field3
      size: len_case_0_field3
  case_0_field3_entries:
    seq:
    - id: case_0_field3_elt_tag
      type: u1
      enum: case_0_field3_elt_tag
    - id: inode
      type: u1
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_0
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_1
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: u2be
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_2
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_3
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_4
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: s4be
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_5
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_6
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_7
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: s8be
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_8
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_9
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: inode
      type: inode_10
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::inode)
    - id: other_elts
      type: other_elts_entries
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_entries
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_0
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_2
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_3
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: bytes_dyn_uint30
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_4
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_5
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_6
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_7
      if: (case_0_field3_elt_tag == case_0_field3_elt_tag::other_elts)
  case_1:
    seq:
    - id: case_1_field0
      type: s2be
    - id: case_1_field1
      size: 32
      doc: context_hash
    - id: case_1_field2
      size: 32
      doc: context_hash
    - id: case_1_field3
      type: case_1_field3_0
  case_1_field3:
    seq:
    - id: case_1_field3_entries
      type: case_1_field3_entries
      repeat: eos
  case_1_field3_0:
    seq:
    - id: len_case_1_field3
      type: u4be
      valid:
        max: 1073741823
    - id: case_1_field3
      type: case_1_field3
      size: len_case_1_field3
  case_1_field3_entries:
    seq:
    - id: case_1_field3_elt_tag
      type: u1
      enum: case_1_field3_elt_tag
    - id: inode
      type: u1
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_0
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_1
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: u2be
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_2
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_3
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_4
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: s4be
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_5
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_6
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_7
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: s8be
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_8
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_9
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: inode
      type: inode_10
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::inode)
    - id: other_elts
      type: other_elts_entries
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_entries
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_0
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_2
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_3
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: bytes_dyn_uint30
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_4
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_5
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_6
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_7
      if: (case_1_field3_elt_tag == case_1_field3_elt_tag::other_elts)
  case_2:
    seq:
    - id: case_2_field0
      type: s2be
    - id: case_2_field1
      size: 32
      doc: context_hash
    - id: case_2_field2
      size: 32
      doc: context_hash
    - id: case_2_field3
      type: case_2_field3_0
  case_2_field3:
    seq:
    - id: case_2_field3_entries
      type: case_2_field3_entries
      repeat: eos
  case_2_field3_0:
    seq:
    - id: len_case_2_field3
      type: u4be
      valid:
        max: 1073741823
    - id: case_2_field3
      type: case_2_field3
      size: len_case_2_field3
  case_2_field3_entries:
    seq:
    - id: case_2_field3_elt_tag
      type: u1
      enum: case_2_field3_elt_tag
    - id: inode
      type: u1
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_0
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_1
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: u2be
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_2
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_3
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_4
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: s4be
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_5
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_6
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_7
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: s8be
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_8
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_9
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: inode
      type: inode_10
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::inode)
    - id: other_elts
      type: other_elts_entries
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_entries
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_0
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_2
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_3
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: bytes_dyn_uint30
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_4
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_5
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_6
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_7
      if: (case_2_field3_elt_tag == case_2_field3_elt_tag::other_elts)
  case_3:
    seq:
    - id: case_3_field0
      type: s2be
    - id: case_3_field1
      size: 32
      doc: context_hash
    - id: case_3_field2
      size: 32
      doc: context_hash
    - id: case_3_field3
      type: case_3_field3_0
  case_3_field3:
    seq:
    - id: case_3_field3_entries
      type: case_3_field3_entries
      repeat: eos
  case_3_field3_0:
    seq:
    - id: len_case_3_field3
      type: u4be
      valid:
        max: 1073741823
    - id: case_3_field3
      type: case_3_field3
      size: len_case_3_field3
  case_3_field3_entries:
    seq:
    - id: case_3_field3_elt_tag
      type: u1
      enum: case_3_field3_elt_tag
    - id: inode
      type: u1
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_0
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_1
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: u2be
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_2
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_3
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_4
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: s4be
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_5
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_6
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_7
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: s8be
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_8
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_9
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: inode
      type: inode_10
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::inode)
    - id: other_elts
      type: other_elts_entries
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_entries
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_0
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_2
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_3
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: bytes_dyn_uint30
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_4
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_5
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_6
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
    - id: other_elts
      type: other_elts_7
      if: (case_3_field3_elt_tag == case_3_field3_elt_tag::other_elts)
  commitment:
    seq:
    - id: level
      type: s4be
    - id: messages
      type: messages_0
    - id: predecessor
      type: predecessor
    - id: inbox_merkle_root
      size: 32
  commitment_0:
    seq:
    - id: compressed_state
      size: 32
    - id: inbox_level
      type: s4be
    - id: predecessor
      size: 32
    - id: number_of_messages
      type: s4be
    - id: number_of_ticks
      type: s4be
  contents_entries:
    seq:
    - id: id_013__ptjakart__operation__alpha__contents
      type: id_013__ptjakart__operation__alpha__contents
  delegation:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, or P256 public key hash
  deposit:
    seq:
    - id: sender
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: destination
      size: 20
    - id: ticket_hash
      size: 32
    - id: amount
      type: amount
  double_baking_evidence:
    seq:
    - id: bh1
      type: bh1_0
    - id: bh2
      type: bh2_0
  double_endorsement_evidence:
    seq:
    - id: op1
      type: op1_0
    - id: op2
      type: op2_0
  double_preendorsement_evidence:
    seq:
    - id: op1
      type: op1_2
    - id: op2
      type: op2_2
  endorsement:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
  id_013__ptjakart__block_header__alpha__full_header:
    seq:
    - id: id_013__ptjakart__block_header__alpha__full_header
      type: block_header__shell
    - id: id_013__ptjakart__block_header__alpha__signed_contents
      type: id_013__ptjakart__block_header__alpha__signed_contents
  id_013__ptjakart__block_header__alpha__signed_contents:
    seq:
    - id: id_013__ptjakart__block_header__alpha__unsigned_contents
      type: id_013__ptjakart__block_header__alpha__unsigned_contents
    - id: signature
      size: 64
  id_013__ptjakart__block_header__alpha__unsigned_contents:
    seq:
    - id: payload_hash
      size: 32
    - id: payload_round
      type: s4be
    - id: proof_of_work_nonce
      size: 8
    - id: seed_nonce_hash_tag
      type: u1
      enum: bool
    - id: seed_nonce_hash
      size: 32
      if: (seed_nonce_hash_tag == bool::true)
    - id: liquidity_baking_toggle_vote
      type: id_013__ptjakart__liquidity_baking_toggle_vote
  id_013__ptjakart__contract_id:
    seq:
    - id: id_013__ptjakart__contract_id_tag
      type: u1
      enum: id_013__ptjakart__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_013__ptjakart__contract_id_tag == id_013__ptjakart__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: originated
      type: originated
      if: (id_013__ptjakart__contract_id_tag == id_013__ptjakart__contract_id_tag::originated)
  id_013__ptjakart__entrypoint:
    seq:
    - id: id_013__ptjakart__entrypoint_tag
      type: u1
      enum: id_013__ptjakart__entrypoint_tag
    - id: named
      type: named_0
      if: (id_013__ptjakart__entrypoint_tag == id_013__ptjakart__entrypoint_tag::named)
  id_013__ptjakart__inlined__endorsement:
    seq:
    - id: id_013__ptjakart__inlined__endorsement
      type: operation__shell_header
    - id: operations
      type: id_013__ptjakart__inlined__endorsement_mempool__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size: 64
      if: (signature_tag == bool::true)
  id_013__ptjakart__inlined__endorsement_mempool__contents:
    seq:
    - id: id_013__ptjakart__inlined__endorsement_mempool__contents_tag
      type: u1
      enum: id_013__ptjakart__inlined__endorsement_mempool__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_013__ptjakart__inlined__endorsement_mempool__contents_tag == id_013__ptjakart__inlined__endorsement_mempool__contents_tag::endorsement)
  id_013__ptjakart__inlined__preendorsement:
    seq:
    - id: id_013__ptjakart__inlined__preendorsement
      type: operation__shell_header
    - id: operations
      type: id_013__ptjakart__inlined__preendorsement__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size: 64
      if: (signature_tag == bool::true)
  id_013__ptjakart__inlined__preendorsement__contents:
    seq:
    - id: id_013__ptjakart__inlined__preendorsement__contents_tag
      type: u1
      enum: id_013__ptjakart__inlined__preendorsement__contents_tag
    - id: preendorsement
      type: preendorsement
      if: (id_013__ptjakart__inlined__preendorsement__contents_tag == id_013__ptjakart__inlined__preendorsement__contents_tag::preendorsement)
  id_013__ptjakart__liquidity_baking_toggle_vote:
    seq:
    - id: id_013__ptjakart__liquidity_baking_toggle_vote
      type: s1
  id_013__ptjakart__mutez:
    seq:
    - id: id_013__ptjakart__mutez
      type: n
  id_013__ptjakart__operation__alpha__contents:
    seq:
    - id: id_013__ptjakart__operation__alpha__contents_tag
      type: u1
      enum: id_013__ptjakart__operation__alpha__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::endorsement)
    - id: preendorsement
      type: preendorsement
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::preendorsement)
    - id: seed_nonce_revelation
      type: seed_nonce_revelation
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::seed_nonce_revelation)
    - id: double_endorsement_evidence
      type: double_endorsement_evidence
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::double_endorsement_evidence)
    - id: double_preendorsement_evidence
      type: double_preendorsement_evidence
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::double_preendorsement_evidence)
    - id: double_baking_evidence
      type: double_baking_evidence
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::double_baking_evidence)
    - id: activate_account
      type: activate_account
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::activate_account)
    - id: proposals
      type: proposals_1
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::proposals)
    - id: ballot
      type: ballot
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::ballot)
    - id: reveal
      type: reveal
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::reveal)
    - id: transaction
      type: transaction
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::transaction)
    - id: origination
      type: origination
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::origination)
    - id: delegation
      type: delegation
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::delegation)
    - id: set_deposits_limit
      type: set_deposits_limit
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::set_deposits_limit)
    - id: failing_noop
      type: bytes_dyn_uint30
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::failing_noop)
    - id: register_global_constant
      type: register_global_constant
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::register_global_constant)
    - id: tx_rollup_origination
      type: tx_rollup_origination
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::tx_rollup_origination)
    - id: tx_rollup_submit_batch
      type: tx_rollup_submit_batch
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::tx_rollup_submit_batch)
    - id: tx_rollup_commit
      type: tx_rollup_commit
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::tx_rollup_commit)
    - id: tx_rollup_return_bond
      type: tx_rollup_return_bond
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::tx_rollup_return_bond)
    - id: tx_rollup_finalize_commitment
      type: tx_rollup_finalize_commitment
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::tx_rollup_finalize_commitment)
    - id: tx_rollup_remove_commitment
      type: tx_rollup_remove_commitment
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::tx_rollup_remove_commitment)
    - id: tx_rollup_rejection
      type: tx_rollup_rejection
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::tx_rollup_rejection)
    - id: tx_rollup_dispatch_tickets
      type: tx_rollup_dispatch_tickets
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::tx_rollup_dispatch_tickets)
    - id: transfer_ticket
      type: transfer_ticket
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::transfer_ticket)
    - id: sc_rollup_originate
      type: sc_rollup_originate
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::sc_rollup_originate)
    - id: sc_rollup_add_messages
      type: sc_rollup_add_messages
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::sc_rollup_add_messages)
    - id: sc_rollup_cement
      type: sc_rollup_cement
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::sc_rollup_cement)
    - id: sc_rollup_publish
      type: sc_rollup_publish
      if: (id_013__ptjakart__operation__alpha__contents_tag == id_013__ptjakart__operation__alpha__contents_tag::sc_rollup_publish)
  id_013__ptjakart__operation__alpha__unsigned_operation:
    seq:
    - id: id_013__ptjakart__operation__alpha__unsigned_operation
      type: operation__shell_header
    - id: contents
      type: contents_entries
      repeat: eos
  id_013__ptjakart__rollup_address:
    seq:
    - id: id_013__ptjakart__rollup_address
      type: bytes_dyn_uint30
  id_013__ptjakart__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  id_013__ptjakart__transaction_destination:
    seq:
    - id: id_013__ptjakart__transaction_destination_tag
      type: u1
      enum: id_013__ptjakart__transaction_destination_tag
    - id: implicit
      type: public_key_hash
      if: (id_013__ptjakart__transaction_destination_tag == id_013__ptjakart__transaction_destination_tag::implicit)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: originated
      type: originated
      if: (id_013__ptjakart__transaction_destination_tag == id_013__ptjakart__transaction_destination_tag::originated)
    - id: tx_rollup
      type: tx_rollup
      if: (id_013__ptjakart__transaction_destination_tag == id_013__ptjakart__transaction_destination_tag::tx_rollup)
  id_013__ptjakart__tx_rollup_id:
    seq:
    - id: rollup_hash
      size: 20
  inode:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field1'
  inode_0:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field0'
  inode_1:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1
  inode_10:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1
  inode_2:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field1'
  inode_3:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field0'
  inode_4:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1
  inode_5:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field1'
  inode_6:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field0'
  inode_7:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1
  inode_8:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field1'
  inode_9:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      size: 32
      doc: ! 'context_hash


        inode_field1_field0'
  inode_field1:
    seq:
    - id: inode_field1_field0
      size: 32
      doc: context_hash
    - id: inode_field1_field1
      size: 32
      doc: context_hash
  int31:
    seq:
    - id: int31
      type: s4be
      valid:
        min: -1073741824
        max: 1073741823
  message:
    seq:
    - id: message_tag
      type: u1
      enum: message_tag
    - id: batch
      type: bytes_dyn_uint30
      if: (message_tag == message_tag::batch)
    - id: deposit
      type: deposit
      if: (message_tag == message_tag::deposit)
  message_0:
    seq:
    - id: message_entries
      type: message_entries
      repeat: eos
  message_1:
    seq:
    - id: len_message
      type: u4be
      valid:
        max: 1073741823
    - id: message
      type: message_0
      size: len_message
  message_entries:
    seq:
    - id: message_elt
      type: bytes_dyn_uint30
  message_path:
    seq:
    - id: message_path_entries
      type: message_path_entries
      repeat: eos
  message_path_0:
    seq:
    - id: len_message_path
      type: u4be
      valid:
        max: 1073741823
    - id: message_path
      type: message_path
      size: len_message_path
  message_path_entries:
    seq:
    - id: inbox_list_hash
      size: 32
  message_result_path:
    seq:
    - id: message_result_path_entries
      type: message_result_path_entries
      repeat: eos
  message_result_path_0:
    seq:
    - id: len_message_result_path
      type: u4be
      valid:
        max: 1073741823
    - id: message_result_path
      type: message_result_path
      size: len_message_result_path
  message_result_path_entries:
    seq:
    - id: message_result_list_hash
      size: 32
  messages:
    seq:
    - id: messages_entries
      type: messages_entries
      repeat: eos
  messages_0:
    seq:
    - id: len_messages
      type: u4be
      valid:
        max: 1073741823
    - id: messages
      type: messages
      size: len_messages
  messages_entries:
    seq:
    - id: message_result_hash
      size: 32
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
  named:
    seq:
    - id: named
      size-eos: true
  named_0:
    seq:
    - id: len_named
      type: u1
      valid:
        max: 31
    - id: named
      type: named
      size: len_named
  op1:
    seq:
    - id: id_013__ptjakart__inlined__endorsement
      type: id_013__ptjakart__inlined__endorsement
  op1_0:
    seq:
    - id: len_op1
      type: u4be
      valid:
        max: 1073741823
    - id: op1
      type: op1
      size: len_op1
  op1_1:
    seq:
    - id: id_013__ptjakart__inlined__preendorsement
      type: id_013__ptjakart__inlined__preendorsement
  op1_2:
    seq:
    - id: len_op1
      type: u4be
      valid:
        max: 1073741823
    - id: op1
      type: op1_1
      size: len_op1
  op2:
    seq:
    - id: id_013__ptjakart__inlined__endorsement
      type: id_013__ptjakart__inlined__endorsement
  op2_0:
    seq:
    - id: len_op2
      type: u4be
      valid:
        max: 1073741823
    - id: op2
      type: op2
      size: len_op2
  op2_1:
    seq:
    - id: id_013__ptjakart__inlined__preendorsement
      type: id_013__ptjakart__inlined__preendorsement
  op2_2:
    seq:
    - id: len_op2
      type: u4be
      valid:
        max: 1073741823
    - id: op2
      type: op2_1
      size: len_op2
  originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: balance
      type: id_013__ptjakart__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: script
      type: id_013__ptjakart__scripted__contracts
  other_elts:
    seq:
    - id: other_elts_entries
      type: other_elts_entries
      repeat: eos
  other_elts_0:
    seq:
    - id: len_other_elts
      type: u4be
      valid:
        max: 1073741823
    - id: other_elts
      type: other_elts
      size: len_other_elts
  other_elts_1:
    seq:
    - id: other_elts
      size-eos: true
  other_elts_2:
    seq:
    - id: len_other_elts
      type: u1
      valid:
        max: 255
    - id: other_elts
      type: other_elts_1
      size: len_other_elts
  other_elts_3:
    seq:
    - id: len_other_elts
      type: u2be
      valid:
        max: 65535
    - id: other_elts
      type: other_elts_1
      size: len_other_elts
  other_elts_4:
    seq:
    - id: other_elts_field0
      type: u1
    - id: other_elts_field1
      type: other_elts_field1_0
    - id: other_elts_field2
      size: 32
      doc: context_hash
  other_elts_5:
    seq:
    - id: other_elts_field0
      type: u2be
    - id: other_elts_field1
      type: other_elts_field1_0
    - id: other_elts_field2
      size: 32
      doc: context_hash
  other_elts_6:
    seq:
    - id: other_elts_field0
      type: s4be
    - id: other_elts_field1
      type: other_elts_field1_0
    - id: other_elts_field2
      size: 32
      doc: context_hash
  other_elts_7:
    seq:
    - id: other_elts_field0
      type: s8be
    - id: other_elts_field1
      type: other_elts_field1_0
    - id: other_elts_field2
      size: 32
      doc: context_hash
  other_elts_elt_field0:
    seq:
    - id: other_elts_elt_field0
      size-eos: true
  other_elts_elt_field0_0:
    seq:
    - id: len_other_elts_elt_field0
      type: u1
      valid:
        max: 255
    - id: other_elts_elt_field0
      type: other_elts_elt_field0
      size: len_other_elts_elt_field0
  other_elts_elt_field1:
    seq:
    - id: other_elts_elt_field1_tag
      type: u1
      enum: other_elts_elt_field1_tag
    - id: value
      size: 32
      if: (other_elts_elt_field1_tag == other_elts_elt_field1_tag::value)
    - id: node
      size: 32
      if: (other_elts_elt_field1_tag == other_elts_elt_field1_tag::node)
  other_elts_entries:
    seq:
    - id: other_elts_elt_field0
      type: other_elts_elt_field0_0
    - id: other_elts_elt_field1
      type: other_elts_elt_field1
  other_elts_field1:
    seq:
    - id: other_elts_field1
      size-eos: true
  other_elts_field1_0:
    seq:
    - id: len_other_elts_field1
      type: u1
      valid:
        max: 255
    - id: other_elts_field1
      type: other_elts_field1
      size: len_other_elts_field1
  parameters:
    seq:
    - id: entrypoint
      type: id_013__ptjakart__entrypoint
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: value
      type: bytes_dyn_uint30
  predecessor:
    seq:
    - id: predecessor_tag
      type: u1
      enum: predecessor_tag
    - id: some
      size: 32
      if: (predecessor_tag == predecessor_tag::some)
  preendorsement:
    seq:
    - id: slot
      type: u2be
    - id: level
      type: s4be
    - id: round
      type: s4be
    - id: block_payload_hash
      size: 32
  previous_message_result:
    seq:
    - id: context_hash
      size: 32
    - id: withdraw_list_hash
      size: 32
  previous_message_result_path:
    seq:
    - id: previous_message_result_path_entries
      type: previous_message_result_path_entries
      repeat: eos
  previous_message_result_path_0:
    seq:
    - id: len_previous_message_result_path
      type: u4be
      valid:
        max: 1073741823
    - id: previous_message_result_path
      type: previous_message_result_path
      size: len_previous_message_result_path
  previous_message_result_path_entries:
    seq:
    - id: message_result_list_hash
      size: 32
  proof:
    seq:
    - id: proof_tag
      type: u1
      enum: proof_tag
    - id: case_0
      type: case_0
      if: (proof_tag == proof_tag::case_0)
    - id: case_2
      type: case_2
      if: (proof_tag == proof_tag::case_2)
    - id: case_1
      type: case_1
      if: (proof_tag == proof_tag::case_1)
    - id: case_3
      type: case_3
      if: (proof_tag == proof_tag::case_3)
  proposals:
    seq:
    - id: proposals_entries
      type: proposals_entries
      repeat: eos
  proposals_0:
    seq:
    - id: len_proposals
      type: u4be
      valid:
        max: 1073741823
    - id: proposals
      type: proposals
      size: len_proposals
  proposals_1:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: period
      type: s4be
    - id: proposals
      type: proposals_0
  proposals_entries:
    seq:
    - id: protocol_hash
      size: 32
  public_key:
    seq:
    - id: public_key_tag
      type: u1
      enum: public_key_tag
    - id: ed25519
      size: 32
      if: (public_key_tag == public_key_tag::ed25519)
    - id: secp256k1
      size: 33
      if: (public_key_tag == public_key_tag::secp256k1)
    - id: p256
      size: 33
      if: (public_key_tag == public_key_tag::p256)
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
  register_global_constant:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: value
      type: bytes_dyn_uint30
  reveal:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_key
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
  sc_rollup_add_messages:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_013__ptjakart__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: message
      type: message_1
  sc_rollup_cement:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_013__ptjakart__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: commitment
      size: 32
  sc_rollup_originate:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: kind
      type: u2
      enum: kind_tag
    - id: boot_sector
      type: bytes_dyn_uint30
  sc_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_013__ptjakart__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: commitment
      type: commitment_0
  seed_nonce_revelation:
    seq:
    - id: level
      type: s4be
    - id: nonce
      size: 32
  set_deposits_limit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: limit_tag
      type: u1
      enum: bool
    - id: limit
      type: id_013__ptjakart__mutez
      if: (limit_tag == bool::true)
  tickets_info:
    seq:
    - id: tickets_info_entries
      type: tickets_info_entries
      repeat: eos
  tickets_info_0:
    seq:
    - id: len_tickets_info
      type: u4be
      valid:
        max: 1073741823
    - id: tickets_info
      type: tickets_info
      size: len_tickets_info
  tickets_info_entries:
    seq:
    - id: contents
      type: bytes_dyn_uint30
    - id: ty
      type: bytes_dyn_uint30
    - id: ticketer
      type: id_013__ptjakart__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: amount
      type: amount
    - id: claimer
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
  transaction:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: id_013__ptjakart__mutez
    - id: destination
      type: id_013__ptjakart__transaction_destination
      doc: ! >-
        A destination of a transaction: A destination notation compatible with the
        contract notation as given to an RPC or inside scripts. Can be a base58 implicit
        contract hash, a base58 originated contract hash, or a base58 originated transaction
        rollup.
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
  transfer_ticket:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: ticket_contents
      type: bytes_dyn_uint30
    - id: ticket_ty
      type: bytes_dyn_uint30
    - id: ticket_ticketer
      type: id_013__ptjakart__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: ticket_amount
      type: n
    - id: destination
      type: id_013__ptjakart__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: entrypoint
      type: bytes_dyn_uint30
  tx_rollup:
    seq:
    - id: id_013__ptjakart__tx_rollup_id
      type: id_013__ptjakart__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: tx_rollup_padding
      size: 1
      doc: This field is for padding, ignore
  tx_rollup_commit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_013__ptjakart__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: commitment
      type: commitment
  tx_rollup_dispatch_tickets:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: tx_rollup
      type: id_013__ptjakart__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: level
      type: s4be
    - id: context_hash
      size: 32
    - id: message_index
      type: int31
    - id: message_result_path
      type: message_result_path_0
    - id: tickets_info
      type: tickets_info_0
  tx_rollup_finalize_commitment:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_013__ptjakart__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
  tx_rollup_rejection:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_013__ptjakart__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: level
      type: s4be
    - id: message
      type: message
    - id: message_position
      type: n
    - id: message_path
      type: message_path_0
    - id: message_result_hash
      size: 32
    - id: message_result_path
      type: message_result_path_0
    - id: previous_message_result
      type: previous_message_result
    - id: previous_message_result_path
      type: previous_message_result_path_0
    - id: proof
      type: proof
  tx_rollup_remove_commitment:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_013__ptjakart__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_return_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_013__ptjakart__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_submit_batch:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_013__ptjakart__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_013__ptjakart__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: content
      type: bytes_dyn_uint30
    - id: burn_limit_tag
      type: u1
      enum: bool
    - id: burn_limit
      type: id_013__ptjakart__mutez
      if: (burn_limit_tag == bool::true)
enums:
  amount_tag:
    0: small
    1: medium
    2: biggish
    3: bigger
  bool:
    0: false
    255: true
  case_0_field3_elt_tag:
    0: inode
    1: inode
    2: inode
    3: inode
    4: inode
    5: inode
    6: inode
    7: inode
    8: inode
    9: inode
    10: inode
    11: inode
    12: inode
    13: inode
    14: inode
    15: inode
    128: other_elts
    129: other_elts
    130: other_elts
    131: other_elts
    192: other_elts
    193: other_elts
    195: other_elts
    224: other_elts
    225: other_elts
    226: other_elts
    227: other_elts
  case_1_field3_elt_tag:
    0: inode
    1: inode
    2: inode
    3: inode
    4: inode
    5: inode
    6: inode
    7: inode
    8: inode
    9: inode
    10: inode
    11: inode
    12: inode
    13: inode
    14: inode
    15: inode
    128: other_elts
    129: other_elts
    130: other_elts
    131: other_elts
    192: other_elts
    193: other_elts
    195: other_elts
    224: other_elts
    225: other_elts
    226: other_elts
    227: other_elts
  case_2_field3_elt_tag:
    0: inode
    1: inode
    2: inode
    3: inode
    4: inode
    5: inode
    6: inode
    7: inode
    8: inode
    9: inode
    10: inode
    11: inode
    12: inode
    13: inode
    14: inode
    15: inode
    128: other_elts
    129: other_elts
    130: other_elts
    131: other_elts
    192: other_elts
    193: other_elts
    195: other_elts
    224: other_elts
    225: other_elts
    226: other_elts
    227: other_elts
  case_3_field3_elt_tag:
    0: inode
    1: inode
    2: inode
    3: inode
    4: inode
    5: inode
    6: inode
    7: inode
    8: inode
    9: inode
    10: inode
    11: inode
    12: inode
    13: inode
    14: inode
    15: inode
    128: other_elts
    129: other_elts
    130: other_elts
    131: other_elts
    192: other_elts
    193: other_elts
    195: other_elts
    224: other_elts
    225: other_elts
    226: other_elts
    227: other_elts
  id_013__ptjakart__contract_id_tag:
    0: implicit
    1: originated
  id_013__ptjakart__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    255: named
  id_013__ptjakart__inlined__endorsement_mempool__contents_tag:
    21: endorsement
  id_013__ptjakart__inlined__preendorsement__contents_tag:
    20: preendorsement
  id_013__ptjakart__operation__alpha__contents_tag:
    1: seed_nonce_revelation
    2: double_endorsement_evidence
    3: double_baking_evidence
    4: activate_account
    5: proposals
    6: ballot
    7: double_preendorsement_evidence
    17: failing_noop
    20: preendorsement
    21: endorsement
    107: reveal
    108: transaction
    109: origination
    110: delegation
    111: register_global_constant
    112: set_deposits_limit
    150: tx_rollup_origination
    151: tx_rollup_submit_batch
    152: tx_rollup_commit
    153: tx_rollup_return_bond
    154: tx_rollup_finalize_commitment
    155: tx_rollup_remove_commitment
    156: tx_rollup_rejection
    157: tx_rollup_dispatch_tickets
    158: transfer_ticket
    200: sc_rollup_originate
    201: sc_rollup_add_messages
    202: sc_rollup_cement
    203: sc_rollup_publish
  id_013__ptjakart__transaction_destination_tag:
    0: implicit
    1: originated
    2: tx_rollup
  kind_tag:
    0: example_arith__smart__contract__rollup__kind
  message_tag:
    0: batch
    1: deposit
  other_elts_elt_field1_tag:
    0: value
    1: node
  predecessor_tag:
    0: none
    1: some
  proof_tag:
    0: case_0
    1: case_1
    2: case_2
    3: case_3
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: id_013__ptjakart__operation__alpha__unsigned_operation
  type: id_013__ptjakart__operation__alpha__unsigned_operation
