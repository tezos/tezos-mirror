meta:
  id: id_015__ptlimapt__operation__contents_list
  endian: be
  imports:
  - block_header__shell
  - operation__shell_header
doc: ! 'Encoding id: 015-PtLimaPt.operation.contents_list'
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
      type: micheline__015__ptlimapt__michelson_v1__expression
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
    - id: id_015__ptlimapt__block_header__alpha__full_header
      type: id_015__ptlimapt__block_header__alpha__full_header
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
    - id: id_015__ptlimapt__block_header__alpha__full_header
      type: id_015__ptlimapt__block_header__alpha__full_header
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
      type: tree_encoding
      doc: tree_encoding
  case_0_0:
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
      type: tree_encoding
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
      type: tree_encoding
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
      type: tree_encoding
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
      type: tree_encoding
  circuits_info:
    seq:
    - id: circuits_info_entries
      type: circuits_info_entries
      repeat: eos
  circuits_info_0:
    seq:
    - id: len_circuits_info
      type: u4be
      valid:
        max: 1073741823
    - id: circuits_info
      type: circuits_info
      size: len_circuits_info
  circuits_info_entries:
    seq:
    - id: circuits_info_elt_field0
      type: bytes_dyn_uint30
    - id: circuits_info_elt_field1
      type: u1
      enum: bool
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
    - id: number_of_ticks
      type: s8be
  dal_publish_slot_header:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: slot
      type: slot
  dal_slot_availability:
    seq:
    - id: endorser
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: endorsement
      type: z
  delegation:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
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
  dissection:
    seq:
    - id: dissection_entries
      type: dissection_entries
      repeat: eos
  dissection_0:
    seq:
    - id: len_dissection
      type: u4be
      valid:
        max: 1073741823
    - id: dissection
      type: dissection
      size: len_dissection
  dissection_entries:
    seq:
    - id: state_tag
      type: u1
      enum: bool
    - id: state
      size: 32
      if: (state_tag == bool::true)
    - id: tick
      type: n
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
  drain_delegate:
    seq:
    - id: consensus_key
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: destination
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
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
  id_015__ptlimapt__block_header__alpha__full_header:
    seq:
    - id: id_015__ptlimapt__block_header__alpha__full_header
      type: block_header__shell
    - id: id_015__ptlimapt__block_header__alpha__signed_contents
      type: id_015__ptlimapt__block_header__alpha__signed_contents
  id_015__ptlimapt__block_header__alpha__signed_contents:
    seq:
    - id: id_015__ptlimapt__block_header__alpha__unsigned_contents
      type: id_015__ptlimapt__block_header__alpha__unsigned_contents
    - id: signature
      size: 64
  id_015__ptlimapt__block_header__alpha__unsigned_contents:
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
      type: id_015__ptlimapt__liquidity_baking_toggle_vote
  id_015__ptlimapt__contract_id:
    seq:
    - id: id_015__ptlimapt__contract_id_tag
      type: u1
      enum: id_015__ptlimapt__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_015__ptlimapt__contract_id_tag == id_015__ptlimapt__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: originated
      type: originated
      if: (id_015__ptlimapt__contract_id_tag == id_015__ptlimapt__contract_id_tag::originated)
  id_015__ptlimapt__contract_id__originated:
    seq:
    - id: id_015__ptlimapt__contract_id__originated_tag
      type: u1
      enum: id_015__ptlimapt__contract_id__originated_tag
    - id: originated
      type: originated
      if: (id_015__ptlimapt__contract_id__originated_tag == id_015__ptlimapt__contract_id__originated_tag::originated)
  id_015__ptlimapt__entrypoint:
    seq:
    - id: id_015__ptlimapt__entrypoint_tag
      type: u1
      enum: id_015__ptlimapt__entrypoint_tag
    - id: named
      type: named_0
      if: (id_015__ptlimapt__entrypoint_tag == id_015__ptlimapt__entrypoint_tag::named)
  id_015__ptlimapt__inlined__endorsement:
    seq:
    - id: id_015__ptlimapt__inlined__endorsement
      type: operation__shell_header
    - id: operations
      type: id_015__ptlimapt__inlined__endorsement_mempool__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size: 64
      if: (signature_tag == bool::true)
  id_015__ptlimapt__inlined__endorsement_mempool__contents:
    seq:
    - id: id_015__ptlimapt__inlined__endorsement_mempool__contents_tag
      type: u1
      enum: id_015__ptlimapt__inlined__endorsement_mempool__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_015__ptlimapt__inlined__endorsement_mempool__contents_tag == id_015__ptlimapt__inlined__endorsement_mempool__contents_tag::endorsement)
  id_015__ptlimapt__inlined__preendorsement:
    seq:
    - id: id_015__ptlimapt__inlined__preendorsement
      type: operation__shell_header
    - id: operations
      type: id_015__ptlimapt__inlined__preendorsement__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size: 64
      if: (signature_tag == bool::true)
  id_015__ptlimapt__inlined__preendorsement__contents:
    seq:
    - id: id_015__ptlimapt__inlined__preendorsement__contents_tag
      type: u1
      enum: id_015__ptlimapt__inlined__preendorsement__contents_tag
    - id: preendorsement
      type: preendorsement
      if: (id_015__ptlimapt__inlined__preendorsement__contents_tag == id_015__ptlimapt__inlined__preendorsement__contents_tag::preendorsement)
  id_015__ptlimapt__liquidity_baking_toggle_vote:
    seq:
    - id: id_015__ptlimapt__liquidity_baking_toggle_vote
      type: s1
  id_015__ptlimapt__michelson__v1__primitives:
    seq:
    - id: id_015__ptlimapt__michelson__v1__primitives
      type: u1
      enum: id_015__ptlimapt__michelson__v1__primitives
  id_015__ptlimapt__mutez:
    seq:
    - id: id_015__ptlimapt__mutez
      type: n
  id_015__ptlimapt__operation__alpha__contents:
    seq:
    - id: id_015__ptlimapt__operation__alpha__contents_tag
      type: u1
      enum: id_015__ptlimapt__operation__alpha__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::endorsement)
    - id: preendorsement
      type: preendorsement
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::preendorsement)
    - id: dal_slot_availability
      type: dal_slot_availability
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::dal_slot_availability)
    - id: seed_nonce_revelation
      type: seed_nonce_revelation
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::seed_nonce_revelation)
    - id: vdf_revelation
      type: solution
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::vdf_revelation)
    - id: double_endorsement_evidence
      type: double_endorsement_evidence
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::double_endorsement_evidence)
    - id: double_preendorsement_evidence
      type: double_preendorsement_evidence
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::double_preendorsement_evidence)
    - id: double_baking_evidence
      type: double_baking_evidence
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::double_baking_evidence)
    - id: activate_account
      type: activate_account
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::activate_account)
    - id: proposals
      type: proposals_1
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::proposals)
    - id: ballot
      type: ballot
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::ballot)
    - id: reveal
      type: reveal
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::reveal)
    - id: transaction
      type: transaction
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::transaction)
    - id: origination
      type: origination
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::origination)
    - id: delegation
      type: delegation
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::delegation)
    - id: set_deposits_limit
      type: set_deposits_limit
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::set_deposits_limit)
    - id: increase_paid_storage
      type: increase_paid_storage
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::increase_paid_storage)
    - id: update_consensus_key
      type: update_consensus_key
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::update_consensus_key)
    - id: drain_delegate
      type: drain_delegate
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::drain_delegate)
    - id: failing_noop
      type: bytes_dyn_uint30
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::failing_noop)
    - id: register_global_constant
      type: register_global_constant
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::register_global_constant)
    - id: tx_rollup_origination
      type: tx_rollup_origination
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_origination)
    - id: tx_rollup_submit_batch
      type: tx_rollup_submit_batch
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_submit_batch)
    - id: tx_rollup_commit
      type: tx_rollup_commit
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_commit)
    - id: tx_rollup_return_bond
      type: tx_rollup_return_bond
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_return_bond)
    - id: tx_rollup_finalize_commitment
      type: tx_rollup_finalize_commitment
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_finalize_commitment)
    - id: tx_rollup_remove_commitment
      type: tx_rollup_remove_commitment
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_remove_commitment)
    - id: tx_rollup_rejection
      type: tx_rollup_rejection
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_rejection)
    - id: tx_rollup_dispatch_tickets
      type: tx_rollup_dispatch_tickets
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::tx_rollup_dispatch_tickets)
    - id: transfer_ticket
      type: transfer_ticket
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::transfer_ticket)
    - id: dal_publish_slot_header
      type: dal_publish_slot_header
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::dal_publish_slot_header)
    - id: sc_rollup_originate
      type: sc_rollup_originate
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_originate)
    - id: sc_rollup_add_messages
      type: sc_rollup_add_messages
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_add_messages)
    - id: sc_rollup_cement
      type: sc_rollup_cement
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_cement)
    - id: sc_rollup_publish
      type: sc_rollup_publish
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_publish)
    - id: sc_rollup_refute
      type: sc_rollup_refute
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_refute)
    - id: sc_rollup_timeout
      type: sc_rollup_timeout
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_timeout)
    - id: sc_rollup_execute_outbox_message
      type: sc_rollup_execute_outbox_message
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_execute_outbox_message)
    - id: sc_rollup_recover_bond
      type: sc_rollup_recover_bond
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_recover_bond)
    - id: sc_rollup_dal_slot_subscribe
      type: sc_rollup_dal_slot_subscribe
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::sc_rollup_dal_slot_subscribe)
    - id: zk_rollup_origination
      type: zk_rollup_origination
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::zk_rollup_origination)
    - id: zk_rollup_publish
      type: zk_rollup_publish
      if: (id_015__ptlimapt__operation__alpha__contents_tag == id_015__ptlimapt__operation__alpha__contents_tag::zk_rollup_publish)
  id_015__ptlimapt__operation__contents_list_entries:
    seq:
    - id: id_015__ptlimapt__operation__alpha__contents
      type: id_015__ptlimapt__operation__alpha__contents
  id_015__ptlimapt__rollup_address:
    seq:
    - id: id_015__ptlimapt__rollup_address
      type: bytes_dyn_uint30
  id_015__ptlimapt__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
  id_015__ptlimapt__tx_rollup_id:
    seq:
    - id: rollup_hash
      size: 20
  inbox__proof:
    seq:
    - id: level
      type: s4be
    - id: message_counter
      type: n
    - id: serialized_proof
      type: bytes_dyn_uint30
  increase_paid_storage:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: z
    - id: destination
      type: id_015__ptlimapt__contract_id__originated
      doc: ! >-
        A contract handle -- originated account: A contract notation as given to an
        RPC or inside scripts. Can be a base58 originated contract hash.
  init_state:
    seq:
    - id: init_state_entries
      type: init_state_entries
      repeat: eos
  init_state_0:
    seq:
    - id: len_init_state
      type: u4be
      valid:
        max: 1073741823
    - id: init_state
      type: init_state
      size: len_init_state
  init_state_entries:
    seq:
    - id: init_state_elt
      size: 32
  inode:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries
      repeat: expr
      repeat-expr: 1
      doc: inode_field1_entries
  inode_0:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 2
      doc: inode_field1_entries
  inode_1:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 3
      doc: inode_field1_entries
  inode_10:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 12
      doc: inode_field1_entries
  inode_11:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 13
      doc: inode_field1_entries
  inode_12:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 14
      doc: inode_field1_entries
  inode_13:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_0
  inode_14:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_1
      repeat: expr
      repeat-expr: 32
      doc: inode_field1_entries
  inode_15:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 1
      doc: inode_field1_entries
  inode_16:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 2
      doc: inode_field1_entries
  inode_17:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 3
      doc: inode_field1_entries
  inode_18:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 4
      doc: inode_field1_entries
  inode_19:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 5
      doc: inode_field1_entries
  inode_2:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 4
      doc: inode_field1_entries
  inode_20:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 6
      doc: inode_field1_entries
  inode_21:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 7
      doc: inode_field1_entries
  inode_22:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 8
      doc: inode_field1_entries
  inode_23:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 9
      doc: inode_field1_entries
  inode_24:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 10
      doc: inode_field1_entries
  inode_25:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 11
      doc: inode_field1_entries
  inode_26:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 12
      doc: inode_field1_entries
  inode_27:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 13
      doc: inode_field1_entries
  inode_28:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 14
      doc: inode_field1_entries
  inode_29:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_0
  inode_3:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 5
      doc: inode_field1_entries
  inode_30:
    seq:
    - id: inode_field0
      type: u2be
    - id: inode_field1
      type: inode_field1_entries_1
      repeat: expr
      repeat-expr: 32
      doc: inode_field1_entries
  inode_31:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 1
      doc: inode_field1_entries
  inode_32:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 2
      doc: inode_field1_entries
  inode_33:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 3
      doc: inode_field1_entries
  inode_34:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 4
      doc: inode_field1_entries
  inode_35:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 5
      doc: inode_field1_entries
  inode_36:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 6
      doc: inode_field1_entries
  inode_37:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 7
      doc: inode_field1_entries
  inode_38:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 8
      doc: inode_field1_entries
  inode_39:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 9
      doc: inode_field1_entries
  inode_4:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 6
      doc: inode_field1_entries
  inode_40:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 10
      doc: inode_field1_entries
  inode_41:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 11
      doc: inode_field1_entries
  inode_42:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 12
      doc: inode_field1_entries
  inode_43:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 13
      doc: inode_field1_entries
  inode_44:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 14
      doc: inode_field1_entries
  inode_45:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_0
  inode_46:
    seq:
    - id: inode_field0
      type: s4be
    - id: inode_field1
      type: inode_field1_entries_1
      repeat: expr
      repeat-expr: 32
      doc: inode_field1_entries
  inode_47:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 1
      doc: inode_field1_entries
  inode_48:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 2
      doc: inode_field1_entries
  inode_49:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 3
      doc: inode_field1_entries
  inode_5:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 7
      doc: inode_field1_entries
  inode_50:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 4
      doc: inode_field1_entries
  inode_51:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 5
      doc: inode_field1_entries
  inode_52:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 6
      doc: inode_field1_entries
  inode_53:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 7
      doc: inode_field1_entries
  inode_54:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 8
      doc: inode_field1_entries
  inode_55:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 9
      doc: inode_field1_entries
  inode_56:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 10
      doc: inode_field1_entries
  inode_57:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 11
      doc: inode_field1_entries
  inode_58:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 12
      doc: inode_field1_entries
  inode_59:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 13
      doc: inode_field1_entries
  inode_6:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 8
      doc: inode_field1_entries
  inode_60:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 14
      doc: inode_field1_entries
  inode_61:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_0
  inode_62:
    seq:
    - id: inode_field0
      type: s8be
    - id: inode_field1
      type: inode_field1_entries_1
      repeat: expr
      repeat-expr: 32
      doc: inode_field1_entries
  inode_7:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 9
      doc: inode_field1_entries
  inode_8:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 10
      doc: inode_field1_entries
  inode_9:
    seq:
    - id: inode_field0
      type: u1
    - id: inode_field1
      type: inode_field1_entries_0
      repeat: expr
      repeat-expr: 11
      doc: inode_field1_entries
  inode_field1:
    seq:
    - id: inode_field1_entries
      type: inode_field1_entries_0
      repeat: eos
  inode_field1_0:
    seq:
    - id: len_inode_field1
      type: u4be
      valid:
        max: 1073741823
    - id: inode_field1
      type: inode_field1
      size: len_inode_field1
  inode_field1_entries:
    seq:
    - id: inode_field1_elt_field0
      type: u1
    - id: inode_field1_elt_field1
      type: inode_tree_63
      doc: inode_tree
  inode_field1_entries_0:
    seq:
    - id: inode_field1_elt_field0
      type: u1
    - id: inode_field1_elt_field1
      type: inode_tree
  inode_field1_entries_1:
    seq:
    - id: inode_field1_elt
      type: inode_tree
  inode_tree:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 1
      doc: inode_tree_field1_entries
  inode_tree_0:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 2
      doc: inode_tree_field1_entries
  inode_tree_1:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 3
      doc: inode_tree_field1_entries
  inode_tree_10:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 12
      doc: inode_tree_field1_entries
  inode_tree_11:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 13
      doc: inode_tree_field1_entries
  inode_tree_12:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 14
      doc: inode_tree_field1_entries
  inode_tree_13:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_0
  inode_tree_14:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries_0
      repeat: expr
      repeat-expr: 32
      doc: inode_tree_field1_entries
  inode_tree_15:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 1
      doc: inode_tree_field1_entries
  inode_tree_16:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 2
      doc: inode_tree_field1_entries
  inode_tree_17:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 3
      doc: inode_tree_field1_entries
  inode_tree_18:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 4
      doc: inode_tree_field1_entries
  inode_tree_19:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 5
      doc: inode_tree_field1_entries
  inode_tree_2:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 4
      doc: inode_tree_field1_entries
  inode_tree_20:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 6
      doc: inode_tree_field1_entries
  inode_tree_21:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 7
      doc: inode_tree_field1_entries
  inode_tree_22:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 8
      doc: inode_tree_field1_entries
  inode_tree_23:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 9
      doc: inode_tree_field1_entries
  inode_tree_24:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 10
      doc: inode_tree_field1_entries
  inode_tree_25:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 11
      doc: inode_tree_field1_entries
  inode_tree_26:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 12
      doc: inode_tree_field1_entries
  inode_tree_27:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 13
      doc: inode_tree_field1_entries
  inode_tree_28:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 14
      doc: inode_tree_field1_entries
  inode_tree_29:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_0
  inode_tree_3:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 5
      doc: inode_tree_field1_entries
  inode_tree_30:
    seq:
    - id: inode_tree_field0
      type: u2be
    - id: inode_tree_field1
      type: inode_tree_field1_entries_0
      repeat: expr
      repeat-expr: 32
      doc: inode_tree_field1_entries
  inode_tree_31:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 1
      doc: inode_tree_field1_entries
  inode_tree_32:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 2
      doc: inode_tree_field1_entries
  inode_tree_33:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 3
      doc: inode_tree_field1_entries
  inode_tree_34:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 4
      doc: inode_tree_field1_entries
  inode_tree_35:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 5
      doc: inode_tree_field1_entries
  inode_tree_36:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 6
      doc: inode_tree_field1_entries
  inode_tree_37:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 7
      doc: inode_tree_field1_entries
  inode_tree_38:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 8
      doc: inode_tree_field1_entries
  inode_tree_39:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 9
      doc: inode_tree_field1_entries
  inode_tree_4:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 6
      doc: inode_tree_field1_entries
  inode_tree_40:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 10
      doc: inode_tree_field1_entries
  inode_tree_41:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 11
      doc: inode_tree_field1_entries
  inode_tree_42:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 12
      doc: inode_tree_field1_entries
  inode_tree_43:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 13
      doc: inode_tree_field1_entries
  inode_tree_44:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 14
      doc: inode_tree_field1_entries
  inode_tree_45:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_0
  inode_tree_46:
    seq:
    - id: inode_tree_field0
      type: s4be
    - id: inode_tree_field1
      type: inode_tree_field1_entries_0
      repeat: expr
      repeat-expr: 32
      doc: inode_tree_field1_entries
  inode_tree_47:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 1
      doc: inode_tree_field1_entries
  inode_tree_48:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 2
      doc: inode_tree_field1_entries
  inode_tree_49:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 3
      doc: inode_tree_field1_entries
  inode_tree_5:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 7
      doc: inode_tree_field1_entries
  inode_tree_50:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 4
      doc: inode_tree_field1_entries
  inode_tree_51:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 5
      doc: inode_tree_field1_entries
  inode_tree_52:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 6
      doc: inode_tree_field1_entries
  inode_tree_53:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 7
      doc: inode_tree_field1_entries
  inode_tree_54:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 8
      doc: inode_tree_field1_entries
  inode_tree_55:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 9
      doc: inode_tree_field1_entries
  inode_tree_56:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 10
      doc: inode_tree_field1_entries
  inode_tree_57:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 11
      doc: inode_tree_field1_entries
  inode_tree_58:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 12
      doc: inode_tree_field1_entries
  inode_tree_59:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 13
      doc: inode_tree_field1_entries
  inode_tree_6:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 8
      doc: inode_tree_field1_entries
  inode_tree_60:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 14
      doc: inode_tree_field1_entries
  inode_tree_61:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_0
  inode_tree_62:
    seq:
    - id: inode_tree_field0
      type: s8be
    - id: inode_tree_field1
      type: inode_tree_field1_entries_0
      repeat: expr
      repeat-expr: 32
      doc: inode_tree_field1_entries
  inode_tree_63:
    seq:
    - id: inode_tree_tag
      type: u1
      enum: inode_tree_tag
    - id: inode_tree
      type: u1
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_0
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_1
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_2
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_3
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_4
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_5
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_6
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_7
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_8
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_9
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_10
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_11
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_12
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_13
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_14
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: u2be
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_15
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_16
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_17
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_18
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_19
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_20
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_21
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_22
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_23
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_24
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_25
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_26
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_27
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_28
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_29
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_30
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: s4be
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_31
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_32
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_33
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_34
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_35
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_36
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_37
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_38
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_39
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_40
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_41
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_42
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_43
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_44
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_45
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_46
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: s8be
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_47
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_48
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_49
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_50
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_51
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_52
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_53
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_54
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_55
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_56
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_57
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_58
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_59
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_60
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_61
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: inode_tree
      type: inode_tree_62
      if: (inode_tree_tag == inode_tree_tag::inode_tree)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_entries
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_0
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      size: 32
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_1
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_2
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_3
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
    - id: other_inode_trees
      type: other_inode_trees_4
      if: (inode_tree_tag == inode_tree_tag::other_inode_trees)
  inode_tree_7:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 9
      doc: inode_tree_field1_entries
  inode_tree_8:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 10
      doc: inode_tree_field1_entries
  inode_tree_9:
    seq:
    - id: inode_tree_field0
      type: u1
    - id: inode_tree_field1
      type: inode_tree_field1_entries
      repeat: expr
      repeat-expr: 11
      doc: inode_tree_field1_entries
  inode_tree_field1:
    seq:
    - id: inode_tree_field1_entries
      type: inode_tree_field1_entries
      repeat: eos
  inode_tree_field1_0:
    seq:
    - id: len_inode_tree_field1
      type: u4be
      valid:
        max: 1073741823
    - id: inode_tree_field1
      type: inode_tree_field1
      size: len_inode_tree_field1
  inode_tree_field1_entries:
    seq:
    - id: inode_tree_field1_elt_field0
      type: u1
    - id: inode_tree_field1_elt_field1
      type: inode_tree
  inode_tree_field1_entries_0:
    seq:
    - id: inode_tree_field1_elt
      type: inode_tree
  input_proof:
    seq:
    - id: input_proof_tag
      type: u1
      enum: input_proof_tag
    - id: inbox__proof
      type: inbox__proof
      if: (input_proof_tag == input_proof_tag::inbox__proof)
    - id: reveal__proof
      type: reveal_proof
      if: (input_proof_tag == input_proof_tag::reveal__proof)
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
  micheline__015__ptlimapt__michelson_v1__expression:
    seq:
    - id: micheline__015__ptlimapt__michelson_v1__expression_tag
      type: u1
      enum: micheline__015__ptlimapt__michelson_v1__expression_tag
    - id: int
      type: z
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::int)
    - id: string
      type: bytes_dyn_uint30
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::string)
    - id: sequence
      type: sequence_0
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::sequence)
    - id: prim__no_args__no_annots
      type: id_015__ptlimapt__michelson__v1__primitives
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__no_args__no_annots)
    - id: prim__no_args__some_annots
      type: prim__no_args__some_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__no_args__some_annots)
    - id: prim__1_arg__no_annots
      type: prim__1_arg__no_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__1_arg__no_annots)
    - id: prim__1_arg__some_annots
      type: prim__1_arg__some_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__1_arg__some_annots)
    - id: prim__2_args__no_annots
      type: prim__2_args__no_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__2_args__no_annots)
    - id: prim__2_args__some_annots
      type: prim__2_args__some_annots
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__2_args__some_annots)
    - id: prim__generic
      type: prim__generic
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::prim__generic)
    - id: bytes
      type: bytes_dyn_uint30
      if: (micheline__015__ptlimapt__michelson_v1__expression_tag == micheline__015__ptlimapt__michelson_v1__expression_tag::bytes)
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
  op:
    seq:
    - id: op_entries
      type: op_entries
      repeat: eos
  op1:
    seq:
    - id: id_015__ptlimapt__inlined__endorsement
      type: id_015__ptlimapt__inlined__endorsement
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
    - id: id_015__ptlimapt__inlined__preendorsement
      type: id_015__ptlimapt__inlined__preendorsement
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
    - id: id_015__ptlimapt__inlined__endorsement
      type: id_015__ptlimapt__inlined__endorsement
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
    - id: id_015__ptlimapt__inlined__preendorsement
      type: id_015__ptlimapt__inlined__preendorsement
  op2_2:
    seq:
    - id: len_op2
      type: u4be
      valid:
        max: 1073741823
    - id: op2
      type: op2_1
      size: len_op2
  op_0:
    seq:
    - id: len_op
      type: u4be
      valid:
        max: 1073741823
    - id: op
      type: op
      size: len_op
  op_elt_field0:
    seq:
    - id: op_code
      type: int31
    - id: price
      type: price
    - id: l1_dst
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: rollup_id
      size: 20
    - id: payload
      type: payload_0
  op_elt_field1:
    seq:
    - id: op_elt_field1_tag
      type: u1
      enum: op_elt_field1_tag
    - id: some
      type: some
      if: (op_elt_field1_tag == op_elt_field1_tag::some)
  op_entries:
    seq:
    - id: op_elt_field0
      type: op_elt_field0
    - id: op_elt_field1
      type: op_elt_field1
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
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: balance
      type: id_015__ptlimapt__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: script
      type: id_015__ptlimapt__scripted__contracts
  other_inode_trees:
    seq:
    - id: other_inode_trees_entries
      type: other_inode_trees_entries
      repeat: eos
  other_inode_trees_0:
    seq:
    - id: len_other_inode_trees
      type: u4be
      valid:
        max: 1073741823
    - id: other_inode_trees
      type: other_inode_trees
      size: len_other_inode_trees
  other_inode_trees_1:
    seq:
    - id: other_inode_trees_field0
      type: u1
    - id: other_inode_trees_field1
      type: other_inode_trees_field1_0
    - id: other_inode_trees_field2
      type: inode_tree
  other_inode_trees_2:
    seq:
    - id: other_inode_trees_field0
      type: u2be
    - id: other_inode_trees_field1
      type: other_inode_trees_field1_0
    - id: other_inode_trees_field2
      type: inode_tree
  other_inode_trees_3:
    seq:
    - id: other_inode_trees_field0
      type: s4be
    - id: other_inode_trees_field1
      type: other_inode_trees_field1_0
    - id: other_inode_trees_field2
      type: inode_tree
  other_inode_trees_4:
    seq:
    - id: other_inode_trees_field0
      type: s8be
    - id: other_inode_trees_field1
      type: other_inode_trees_field1_0
    - id: other_inode_trees_field2
      type: inode_tree
  other_inode_trees_elt_field0:
    seq:
    - id: other_inode_trees_elt_field0
      size-eos: true
  other_inode_trees_elt_field0_0:
    seq:
    - id: len_other_inode_trees_elt_field0
      type: u1
      valid:
        max: 255
    - id: other_inode_trees_elt_field0
      type: other_inode_trees_elt_field0
      size: len_other_inode_trees_elt_field0
  other_inode_trees_entries:
    seq:
    - id: other_inode_trees_elt_field0
      type: other_inode_trees_elt_field0_0
    - id: other_inode_trees_elt_field1
      type: tree_encoding
  other_inode_trees_field1:
    seq:
    - id: other_inode_trees_field1
      size-eos: true
  other_inode_trees_field1_0:
    seq:
    - id: len_other_inode_trees_field1
      type: u1
      valid:
        max: 255
    - id: other_inode_trees_field1
      type: other_inode_trees_field1
      size: len_other_inode_trees_field1
  other_trees:
    seq:
    - id: other_trees_entries
      type: other_trees_entries
      repeat: eos
  other_trees_0:
    seq:
    - id: len_other_trees
      type: u4be
      valid:
        max: 1073741823
    - id: other_trees
      type: other_trees
      size: len_other_trees
  other_trees_1:
    seq:
    - id: other_trees
      size-eos: true
  other_trees_2:
    seq:
    - id: len_other_trees
      type: u1
      valid:
        max: 255
    - id: other_trees
      type: other_trees_1
      size: len_other_trees
  other_trees_3:
    seq:
    - id: len_other_trees
      type: u2be
      valid:
        max: 65535
    - id: other_trees
      type: other_trees_1
      size: len_other_trees
  other_trees_4:
    seq:
    - id: other_trees_field0
      type: u1
    - id: other_trees_field1
      type: other_trees_field1_0
    - id: other_trees_field2
      type: inode_tree
  other_trees_5:
    seq:
    - id: other_trees_field0
      type: u2be
    - id: other_trees_field1
      type: other_trees_field1_0
    - id: other_trees_field2
      type: inode_tree
  other_trees_6:
    seq:
    - id: other_trees_field0
      type: s4be
    - id: other_trees_field1
      type: other_trees_field1_0
    - id: other_trees_field2
      type: inode_tree
  other_trees_7:
    seq:
    - id: other_trees_field0
      type: s8be
    - id: other_trees_field1
      type: other_trees_field1_0
    - id: other_trees_field2
      type: inode_tree
  other_trees_elt_field0:
    seq:
    - id: other_trees_elt_field0
      size-eos: true
  other_trees_elt_field0_0:
    seq:
    - id: len_other_trees_elt_field0
      type: u1
      valid:
        max: 255
    - id: other_trees_elt_field0
      type: other_trees_elt_field0
      size: len_other_trees_elt_field0
  other_trees_entries:
    seq:
    - id: other_trees_elt_field0
      type: other_trees_elt_field0_0
    - id: other_trees_elt_field1
      type: tree_encoding
  other_trees_field1:
    seq:
    - id: other_trees_field1
      size-eos: true
  other_trees_field1_0:
    seq:
    - id: len_other_trees_field1
      type: u1
      valid:
        max: 255
    - id: other_trees_field1
      type: other_trees_field1
      size: len_other_trees_field1
  parameters:
    seq:
    - id: entrypoint
      type: id_015__ptlimapt__entrypoint
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: value
      type: bytes_dyn_uint30
  payload:
    seq:
    - id: payload_entries
      type: payload_entries
      repeat: eos
  payload_0:
    seq:
    - id: len_payload
      type: u4be
      valid:
        max: 1073741823
    - id: payload
      type: payload
      size: len_payload
  payload_entries:
    seq:
    - id: payload_elt
      size: 32
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
  price:
    seq:
    - id: id
      size: 32
    - id: amount
      type: z
  prim__1_arg__no_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: arg
      type: micheline__015__ptlimapt__michelson_v1__expression
  prim__1_arg__some_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: arg
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__2_args__no_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: arg1
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: arg2
      type: micheline__015__ptlimapt__michelson_v1__expression
  prim__2_args__some_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: arg1
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: arg2
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: annots
      type: bytes_dyn_uint30
  prim__generic:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: args
      type: args_0
    - id: annots
      type: bytes_dyn_uint30
  prim__no_args__some_annots:
    seq:
    - id: prim
      type: id_015__ptlimapt__michelson__v1__primitives
    - id: annots
      type: bytes_dyn_uint30
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
  proof_0:
    seq:
    - id: proof_tag
      type: u1
      enum: proof_tag
    - id: case_0
      type: case_0_0
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
  proof_1:
    seq:
    - id: pvm_step
      type: pvm_step
    - id: input_proof_tag
      type: u1
      enum: bool
    - id: input_proof
      type: input_proof
      if: (input_proof_tag == bool::true)
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
        max: 640
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
  public_parameters:
    seq:
    - id: public_parameters_field0
      type: bytes_dyn_uint30
    - id: public_parameters_field1
      type: bytes_dyn_uint30
  pvm_step:
    seq:
    - id: pvm_step_tag
      type: u1
      enum: pvm_step_tag
    - id: arithmetic__pvm__with__proof
      type: proof
      if: (pvm_step_tag == pvm_step_tag::arithmetic__pvm__with__proof)
    - id: wasm__2__0__0__pvm__with__proof
      type: proof_0
      if: (pvm_step_tag == pvm_step_tag::wasm__2__0__0__pvm__with__proof)
  refutation:
    seq:
    - id: choice
      type: n
    - id: step
      type: step
  register_global_constant:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
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
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_key
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
  reveal_proof:
    seq:
    - id: reveal_proof_tag
      type: u1
      enum: reveal_proof_tag
    - id: raw__data__proof
      type: bytes_dyn_uint30
      if: (reveal_proof_tag == reveal_proof_tag::raw__data__proof)
  sc_rollup_add_messages:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
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
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: commitment
      size: 32
  sc_rollup_dal_slot_subscribe:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: slot_index
      type: u1
  sc_rollup_execute_outbox_message:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: cemented_commitment
      size: 32
    - id: output_proof
      type: bytes_dyn_uint30
  sc_rollup_originate:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pvm_kind
      type: u1
      enum: pvm_kind
    - id: boot_sector
      type: bytes_dyn_uint30
    - id: origination_proof
      type: bytes_dyn_uint30
    - id: parameters_ty
      type: bytes_dyn_uint30
  sc_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: commitment
      type: commitment_0
  sc_rollup_recover_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
  sc_rollup_refute:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: opponent
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: refutation_tag
      type: u1
      enum: bool
    - id: refutation
      type: refutation
      if: (refutation_tag == bool::true)
  sc_rollup_timeout:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__rollup_address
      doc: ! >-
        A smart contract rollup address: A smart contract rollup is identified by
        a base58 address starting with scr1
    - id: stakers
      type: stakers
  seed_nonce_revelation:
    seq:
    - id: level
      type: s4be
    - id: nonce
      size: 32
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
      type: micheline__015__ptlimapt__michelson_v1__expression
  set_deposits_limit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
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
      type: id_015__ptlimapt__mutez
      if: (limit_tag == bool::true)
  slot:
    seq:
    - id: level
      type: s4be
    - id: index
      type: u1
    - id: header
      size: 48
  solution:
    seq:
    - id: solution_field0
      size: 100
    - id: solution_field1
      size: 100
  some:
    seq:
    - id: contents
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: ty
      type: micheline__015__ptlimapt__michelson_v1__expression
    - id: ticketer
      type: id_015__ptlimapt__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
  stakers:
    seq:
    - id: alice
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: bob
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
  step:
    seq:
    - id: step_tag
      type: u1
      enum: step_tag
    - id: dissection
      type: dissection_0
      if: (step_tag == step_tag::dissection)
    - id: proof
      type: proof_1
      if: (step_tag == step_tag::proof)
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
      type: id_015__ptlimapt__contract_id
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
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: id_015__ptlimapt__mutez
    - id: destination
      type: id_015__ptlimapt__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
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
      type: id_015__ptlimapt__mutez
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
      type: id_015__ptlimapt__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: ticket_amount
      type: n
    - id: destination
      type: id_015__ptlimapt__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: entrypoint
      type: bytes_dyn_uint30
  tree_encoding:
    seq:
    - id: tree_encoding_tag
      type: u1
      enum: tree_encoding_tag
    - id: inode
      type: u1
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_0
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_1
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_2
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_3
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_4
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_5
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_6
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_7
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_8
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_9
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_10
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_11
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_12
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_13
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_14
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: u2be
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_15
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_16
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_17
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_18
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_19
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_20
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_21
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_22
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_23
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_24
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_25
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_26
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_27
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_28
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_29
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_30
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: s4be
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_31
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_32
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_33
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_34
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_35
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_36
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_37
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_38
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_39
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_40
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_41
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_42
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_43
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_44
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_45
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_46
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: s8be
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_47
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_48
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_49
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_50
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_51
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_52
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_53
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_54
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_55
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_56
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_57
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_58
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_59
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_60
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_61
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: inode
      type: inode_62
      if: (tree_encoding_tag == tree_encoding_tag::inode)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_entries
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_0
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_2
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_3
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: bytes_dyn_uint30
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      size: 32
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      size: 32
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_4
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_5
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_6
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
    - id: other_trees
      type: other_trees_7
      if: (tree_encoding_tag == tree_encoding_tag::other_trees)
  tx_rollup_commit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
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
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: tx_rollup
      type: id_015__ptlimapt__tx_rollup_id
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
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
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
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
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
      type: bytes_dyn_uint30
  tx_rollup_remove_commitment:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_return_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_submit_batch:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_015__ptlimapt__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: content
      type: bytes_dyn_uint30
    - id: burn_limit_tag
      type: u1
      enum: bool
    - id: burn_limit
      type: id_015__ptlimapt__mutez
      if: (burn_limit_tag == bool::true)
  update_consensus_key:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pk
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
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
  zk_rollup_origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_parameters
      type: public_parameters
    - id: circuits_info
      type: circuits_info_0
    - id: init_state
      type: init_state_0
    - id: nb_ops
      type: int31
  zk_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: fee
      type: id_015__ptlimapt__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: zk_rollup
      size: 20
    - id: op
      type: op_0
enums:
  amount_tag:
    0: small
    1: medium
    2: biggish
    3: bigger
  bool:
    0: false
    255: true
  id_015__ptlimapt__contract_id__originated_tag:
    1: originated
  id_015__ptlimapt__contract_id_tag:
    0: implicit
    1: originated
  id_015__ptlimapt__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    5: deposit
    255: named
  id_015__ptlimapt__inlined__endorsement_mempool__contents_tag:
    21: endorsement
  id_015__ptlimapt__inlined__preendorsement__contents_tag:
    20: preendorsement
  id_015__ptlimapt__michelson__v1__primitives:
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
      id: unit_0
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
      id: unit_1
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
      id: lambda_rec
      doc: Lambda_rec
    153:
      id: lambda_rec_0
      doc: LAMBDA_REC
    154:
      id: ticket_0
      doc: TICKET
  id_015__ptlimapt__operation__alpha__contents_tag:
    1: seed_nonce_revelation
    2: double_endorsement_evidence
    3: double_baking_evidence
    4: activate_account
    5: proposals
    6: ballot
    7: double_preendorsement_evidence
    8: vdf_revelation
    9: drain_delegate
    17: failing_noop
    20: preendorsement
    21: endorsement
    22: dal_slot_availability
    107: reveal
    108: transaction
    109: origination
    110: delegation
    111: register_global_constant
    112: set_deposits_limit
    113: increase_paid_storage
    114: update_consensus_key
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
    204: sc_rollup_refute
    205: sc_rollup_timeout
    206: sc_rollup_execute_outbox_message
    207: sc_rollup_recover_bond
    208: sc_rollup_dal_slot_subscribe
    230: dal_publish_slot_header
    250: zk_rollup_origination
    251: zk_rollup_publish
  inode_tree_tag:
    0: inode_tree
    1: inode_tree
    2: inode_tree
    3: inode_tree
    4: inode_tree
    5: inode_tree
    6: inode_tree
    7: inode_tree
    8: inode_tree
    9: inode_tree
    10: inode_tree
    11: inode_tree
    12: inode_tree
    13: inode_tree
    14: inode_tree
    15: inode_tree
    16: inode_tree
    17: inode_tree
    18: inode_tree
    19: inode_tree
    20: inode_tree
    21: inode_tree
    22: inode_tree
    23: inode_tree
    24: inode_tree
    25: inode_tree
    26: inode_tree
    27: inode_tree
    28: inode_tree
    29: inode_tree
    30: inode_tree
    31: inode_tree
    32: inode_tree
    33: inode_tree
    34: inode_tree
    35: inode_tree
    36: inode_tree
    37: inode_tree
    38: inode_tree
    39: inode_tree
    40: inode_tree
    41: inode_tree
    42: inode_tree
    43: inode_tree
    44: inode_tree
    45: inode_tree
    46: inode_tree
    47: inode_tree
    48: inode_tree
    49: inode_tree
    50: inode_tree
    51: inode_tree
    52: inode_tree
    53: inode_tree
    54: inode_tree
    55: inode_tree
    56: inode_tree
    57: inode_tree
    58: inode_tree
    59: inode_tree
    60: inode_tree
    61: inode_tree
    62: inode_tree
    63: inode_tree
    64: inode_tree
    65: inode_tree
    66: inode_tree
    67: inode_tree
    128: other_inode_trees
    129: other_inode_trees
    130: other_inode_trees
    131: other_inode_trees
    132: other_inode_trees
    133: other_inode_trees
    134: other_inode_trees
    135: other_inode_trees
    136: other_inode_trees
    137: other_inode_trees
    138: other_inode_trees
    139: other_inode_trees
    140: other_inode_trees
    141: other_inode_trees
    142: other_inode_trees
    143: other_inode_trees
    144: other_inode_trees
    145: other_inode_trees
    146: other_inode_trees
    147: other_inode_trees
    148: other_inode_trees
    149: other_inode_trees
    150: other_inode_trees
    151: other_inode_trees
    152: other_inode_trees
    153: other_inode_trees
    154: other_inode_trees
    155: other_inode_trees
    156: other_inode_trees
    157: other_inode_trees
    158: other_inode_trees
    159: other_inode_trees
    160: other_inode_trees
    161: other_inode_trees
    162: other_inode_trees
    163: other_inode_trees
    164: other_inode_trees
    165: other_inode_trees
    166: other_inode_trees
    167: other_inode_trees
    168: other_inode_trees
    169: other_inode_trees
    170: other_inode_trees
    171: other_inode_trees
    172: other_inode_trees
    173: other_inode_trees
    174: other_inode_trees
    175: other_inode_trees
    176: other_inode_trees
    177: other_inode_trees
    178: other_inode_trees
    179: other_inode_trees
    180: other_inode_trees
    181: other_inode_trees
    182: other_inode_trees
    183: other_inode_trees
    184: other_inode_trees
    185: other_inode_trees
    186: other_inode_trees
    187: other_inode_trees
    188: other_inode_trees
    189: other_inode_trees
    190: other_inode_trees
    191: other_inode_trees
    192: other_inode_trees
    208: other_inode_trees
    209: other_inode_trees
    210: other_inode_trees
    211: other_inode_trees
    224: other_inode_trees
  input_proof_tag:
    0: inbox__proof
    1: reveal__proof
  message_tag:
    0: batch
    1: deposit
  micheline__015__ptlimapt__michelson_v1__expression_tag:
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
  op_elt_field1_tag:
    0: none
    1: some
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
  pvm_kind:
    0: arith_pvm_kind
    1: wasm_2_0_0_pvm_kind
  pvm_step_tag:
    0: arithmetic__pvm__with__proof
    1: wasm__2__0__0__pvm__with__proof
    255: unencodable
  reveal_proof_tag:
    0: raw__data__proof
  step_tag:
    0: dissection
    1: proof
  tree_encoding_tag:
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
    16: inode
    17: inode
    18: inode
    19: inode
    20: inode
    21: inode
    22: inode
    23: inode
    24: inode
    25: inode
    26: inode
    27: inode
    28: inode
    29: inode
    30: inode
    31: inode
    32: inode
    33: inode
    34: inode
    35: inode
    36: inode
    37: inode
    38: inode
    39: inode
    40: inode
    41: inode
    42: inode
    43: inode
    44: inode
    45: inode
    46: inode
    47: inode
    48: inode
    49: inode
    50: inode
    51: inode
    52: inode
    53: inode
    54: inode
    55: inode
    56: inode
    57: inode
    58: inode
    59: inode
    60: inode
    61: inode
    62: inode
    63: inode
    64: inode
    65: inode
    66: inode
    67: inode
    128: other_trees
    129: other_trees
    130: other_trees
    131: other_trees
    132: other_trees
    133: other_trees
    134: other_trees
    135: other_trees
    136: other_trees
    137: other_trees
    138: other_trees
    139: other_trees
    140: other_trees
    141: other_trees
    142: other_trees
    143: other_trees
    144: other_trees
    145: other_trees
    146: other_trees
    147: other_trees
    148: other_trees
    149: other_trees
    150: other_trees
    151: other_trees
    152: other_trees
    153: other_trees
    154: other_trees
    155: other_trees
    156: other_trees
    157: other_trees
    158: other_trees
    159: other_trees
    160: other_trees
    161: other_trees
    162: other_trees
    163: other_trees
    164: other_trees
    165: other_trees
    166: other_trees
    167: other_trees
    168: other_trees
    169: other_trees
    170: other_trees
    171: other_trees
    172: other_trees
    173: other_trees
    174: other_trees
    175: other_trees
    176: other_trees
    177: other_trees
    178: other_trees
    179: other_trees
    180: other_trees
    181: other_trees
    182: other_trees
    183: other_trees
    184: other_trees
    185: other_trees
    186: other_trees
    187: other_trees
    188: other_trees
    189: other_trees
    190: other_trees
    191: other_trees
    192: other_trees
    193: other_trees
    195: other_trees
    200: other_trees
    208: other_trees
    216: other_trees
    217: other_trees
    218: other_trees
    219: other_trees
    224: other_trees
seq:
- id: id_015__ptlimapt__operation__contents_list_entries
  type: id_015__ptlimapt__operation__contents_list_entries
  repeat: eos
