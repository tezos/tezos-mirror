meta:
  id: id_016__ptmumbai__operation__unsigned
  endian: be
  imports:
  - block_header__shell
  - operation__shell_header
doc: ! 'Encoding id: 016-PtMumbai.operation.unsigned'
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
    - id: case__0
      type: u1
      if: (amount_tag == amount_tag::case__0)
    - id: case__1
      type: u2
      if: (amount_tag == amount_tag::case__1)
    - id: case__2
      type: s4
      if: (amount_tag == amount_tag::case__2)
    - id: case__3
      type: s8
      if: (amount_tag == amount_tag::case__3)
  annots:
    seq:
    - id: len_annots
      type: s4
    - id: annots
      size: len_annots
  arbitrary:
    seq:
    - id: len_arbitrary
      type: s4
    - id: arbitrary
      size: len_arbitrary
  args:
    seq:
    - id: args_entries
      type: args_entries
      repeat: eos
  args_0:
    seq:
    - id: len_args
      type: s4
    - id: args
      type: args
      size: len_args
  args_entries:
    seq:
    - id: args_elt
      type: micheline__016__ptmumbai__michelson_v1__expression
  ballot:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: period
      type: s4
    - id: proposal
      size: 32
    - id: ballot
      type: s1
  batch:
    seq:
    - id: len_batch
      type: s4
    - id: batch
      size: len_batch
  bh1:
    seq:
    - id: id_016__ptmumbai__block_header__alpha__full_header
      type: id_016__ptmumbai__block_header__alpha__full_header
  bh1_0:
    seq:
    - id: len_bh1
      type: s4
    - id: bh1
      type: bh1
      size: len_bh1
  bh2:
    seq:
    - id: id_016__ptmumbai__block_header__alpha__full_header
      type: id_016__ptmumbai__block_header__alpha__full_header
  bh2_0:
    seq:
    - id: len_bh2
      type: s4
    - id: bh2
      type: bh2
      size: len_bh2
  bytes:
    seq:
    - id: len_bytes
      type: s4
    - id: bytes
      size: len_bytes
  circuits_info:
    seq:
    - id: circuits_info_entries
      type: circuits_info_entries
      repeat: eos
  circuits_info_0:
    seq:
    - id: len_circuits_info
      type: s4
    - id: circuits_info
      type: circuits_info
      size: len_circuits_info
  circuits_info_elt_field0:
    seq:
    - id: len_circuits_info_elt_field0
      type: s4
    - id: circuits_info_elt_field0
      size: len_circuits_info_elt_field0
  circuits_info_entries:
    seq:
    - id: circuits_info_elt_field0
      type: circuits_info_elt_field0
    - id: circuits_info_elt_field1
      type: u1
      enum: circuits_info_elt_field1_tag
      doc: circuits_info_elt_field1_tag
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  commitment:
    seq:
    - id: level
      type: s4
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
      type: s4
    - id: predecessor
      size: 32
    - id: number_of_ticks
      type: s8
  content:
    seq:
    - id: len_content
      type: s4
    - id: content
      size: len_content
  contents:
    seq:
    - id: len_contents
      type: s4
    - id: contents
      size: len_contents
  contents_entries:
    seq:
    - id: id_016__ptmumbai__operation__alpha__contents
      type: id_016__ptmumbai__operation__alpha__contents
  dal__page__proof:
    seq:
    - id: dal_page_id
      type: dal_page_id
    - id: dal_proof
      type: dal_proof
  dal_attestation:
    seq:
    - id: attestor
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: attestation
      type: z
    - id: level
      type: s4
  dal_page_id:
    seq:
    - id: published_level
      type: s4
    - id: slot_index
      type: u1
    - id: page_index
      type: s2
  dal_proof:
    seq:
    - id: len_dal_proof
      type: s4
    - id: dal_proof
      size: len_dal_proof
  dal_publish_slot_header:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: slot_header
      type: slot_header
  delegation:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
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
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  deposit:
    seq:
    - id: sender
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
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
      type: s4
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
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: delegate
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: destination
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  endorsement:
    seq:
    - id: slot
      type: u2
    - id: level
      type: s4
    - id: round
      type: s4
    - id: block_payload_hash
      size: 32
  entrypoint:
    seq:
    - id: len_entrypoint
      type: s4
    - id: entrypoint
      size: len_entrypoint
  id_016__ptmumbai__block_header__alpha__full_header:
    seq:
    - id: id_016__ptmumbai__block_header__alpha__full_header
      type: block_header__shell
    - id: id_016__ptmumbai__block_header__alpha__signed_contents
      type: id_016__ptmumbai__block_header__alpha__signed_contents
  id_016__ptmumbai__block_header__alpha__signed_contents:
    seq:
    - id: id_016__ptmumbai__block_header__alpha__unsigned_contents
      type: id_016__ptmumbai__block_header__alpha__unsigned_contents
    - id: signature
      size-eos: true
  id_016__ptmumbai__block_header__alpha__unsigned_contents:
    seq:
    - id: payload_hash
      size: 32
    - id: payload_round
      type: s4
    - id: proof_of_work_nonce
      size: 8
    - id: seed_nonce_hash_tag
      type: u1
      enum: bool
    - id: seed_nonce_hash
      size: 32
      if: (seed_nonce_hash_tag == bool::true)
    - id: liquidity_baking_toggle_vote
      type: id_016__ptmumbai__liquidity_baking_toggle_vote
  id_016__ptmumbai__contract_id:
    seq:
    - id: id_016__ptmumbai__contract_id_tag
      type: u1
      enum: id_016__ptmumbai__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_016__ptmumbai__contract_id_tag == id_016__ptmumbai__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: originated
      type: originated
      if: (id_016__ptmumbai__contract_id_tag == id_016__ptmumbai__contract_id_tag::originated)
  id_016__ptmumbai__contract_id__originated:
    seq:
    - id: id_016__ptmumbai__contract_id__originated_tag
      type: u1
      enum: id_016__ptmumbai__contract_id__originated_tag
    - id: originated
      type: originated
      if: (id_016__ptmumbai__contract_id__originated_tag == id_016__ptmumbai__contract_id__originated_tag::originated)
  id_016__ptmumbai__entrypoint:
    seq:
    - id: id_016__ptmumbai__entrypoint_tag
      type: u1
      enum: id_016__ptmumbai__entrypoint_tag
    - id: named
      type: named_0
      if: (id_016__ptmumbai__entrypoint_tag == id_016__ptmumbai__entrypoint_tag::named)
  id_016__ptmumbai__inlined__endorsement:
    seq:
    - id: id_016__ptmumbai__inlined__endorsement
      type: operation__shell_header
    - id: operations
      type: id_016__ptmumbai__inlined__endorsement_mempool__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size-eos: true
      if: (signature_tag == bool::true)
  id_016__ptmumbai__inlined__endorsement_mempool__contents:
    seq:
    - id: id_016__ptmumbai__inlined__endorsement_mempool__contents_tag
      type: u1
      enum: id_016__ptmumbai__inlined__endorsement_mempool__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_016__ptmumbai__inlined__endorsement_mempool__contents_tag == id_016__ptmumbai__inlined__endorsement_mempool__contents_tag::endorsement)
  id_016__ptmumbai__inlined__preendorsement:
    seq:
    - id: id_016__ptmumbai__inlined__preendorsement
      type: operation__shell_header
    - id: operations
      type: id_016__ptmumbai__inlined__preendorsement__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size-eos: true
      if: (signature_tag == bool::true)
  id_016__ptmumbai__inlined__preendorsement__contents:
    seq:
    - id: id_016__ptmumbai__inlined__preendorsement__contents_tag
      type: u1
      enum: id_016__ptmumbai__inlined__preendorsement__contents_tag
    - id: preendorsement
      type: preendorsement
      if: (id_016__ptmumbai__inlined__preendorsement__contents_tag == id_016__ptmumbai__inlined__preendorsement__contents_tag::preendorsement)
  id_016__ptmumbai__liquidity_baking_toggle_vote:
    seq:
    - id: id_016__ptmumbai__liquidity_baking_toggle_vote
      type: s1
  id_016__ptmumbai__michelson__v1__primitives:
    seq:
    - id: id_016__ptmumbai__michelson__v1__primitives
      type: u1
      enum: id_016__ptmumbai__michelson__v1__primitives
  id_016__ptmumbai__mutez:
    seq:
    - id: id_016__ptmumbai__mutez
      type: n
  id_016__ptmumbai__operation__alpha__contents:
    seq:
    - id: id_016__ptmumbai__operation__alpha__contents_tag
      type: u1
      enum: id_016__ptmumbai__operation__alpha__contents_tag
    - id: endorsement
      type: endorsement
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::endorsement)
    - id: preendorsement
      type: preendorsement
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::preendorsement)
    - id: dal_attestation
      type: dal_attestation
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::dal_attestation)
    - id: seed_nonce_revelation
      type: seed_nonce_revelation
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::seed_nonce_revelation)
    - id: vdf_revelation
      type: solution
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::vdf_revelation)
    - id: double_endorsement_evidence
      type: double_endorsement_evidence
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::double_endorsement_evidence)
    - id: double_preendorsement_evidence
      type: double_preendorsement_evidence
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::double_preendorsement_evidence)
    - id: double_baking_evidence
      type: double_baking_evidence
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::double_baking_evidence)
    - id: activate_account
      type: activate_account
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::activate_account)
    - id: proposals
      type: proposals_1
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::proposals)
    - id: ballot
      type: ballot
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::ballot)
    - id: reveal
      type: reveal
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::reveal)
    - id: transaction
      type: transaction
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::transaction)
    - id: origination
      type: origination
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::origination)
    - id: delegation
      type: delegation
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::delegation)
    - id: set_deposits_limit
      type: set_deposits_limit
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::set_deposits_limit)
    - id: increase_paid_storage
      type: increase_paid_storage
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::increase_paid_storage)
    - id: update_consensus_key
      type: update_consensus_key
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::update_consensus_key)
    - id: drain_delegate
      type: drain_delegate
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::drain_delegate)
    - id: failing_noop
      type: arbitrary
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::failing_noop)
    - id: register_global_constant
      type: register_global_constant
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::register_global_constant)
    - id: tx_rollup_origination
      type: tx_rollup_origination
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::tx_rollup_origination)
    - id: tx_rollup_submit_batch
      type: tx_rollup_submit_batch
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::tx_rollup_submit_batch)
    - id: tx_rollup_commit
      type: tx_rollup_commit
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::tx_rollup_commit)
    - id: tx_rollup_return_bond
      type: tx_rollup_return_bond
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::tx_rollup_return_bond)
    - id: tx_rollup_finalize_commitment
      type: tx_rollup_finalize_commitment
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::tx_rollup_finalize_commitment)
    - id: tx_rollup_remove_commitment
      type: tx_rollup_remove_commitment
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::tx_rollup_remove_commitment)
    - id: tx_rollup_rejection
      type: tx_rollup_rejection
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::tx_rollup_rejection)
    - id: tx_rollup_dispatch_tickets
      type: tx_rollup_dispatch_tickets
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::tx_rollup_dispatch_tickets)
    - id: transfer_ticket
      type: transfer_ticket
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::transfer_ticket)
    - id: dal_publish_slot_header
      type: dal_publish_slot_header
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::dal_publish_slot_header)
    - id: smart_rollup_originate
      type: smart_rollup_originate
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::smart_rollup_originate)
    - id: smart_rollup_add_messages
      type: smart_rollup_add_messages
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::smart_rollup_add_messages)
    - id: smart_rollup_cement
      type: smart_rollup_cement
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::smart_rollup_cement)
    - id: smart_rollup_publish
      type: smart_rollup_publish
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::smart_rollup_publish)
    - id: smart_rollup_refute
      type: smart_rollup_refute
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::smart_rollup_refute)
    - id: smart_rollup_timeout
      type: smart_rollup_timeout
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::smart_rollup_timeout)
    - id: smart_rollup_execute_outbox_message
      type: smart_rollup_execute_outbox_message
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::smart_rollup_execute_outbox_message)
    - id: smart_rollup_recover_bond
      type: smart_rollup_recover_bond
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::smart_rollup_recover_bond)
    - id: zk_rollup_origination
      type: zk_rollup_origination
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::zk_rollup_origination)
    - id: zk_rollup_publish
      type: zk_rollup_publish
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::zk_rollup_publish)
    - id: zk_rollup_update
      type: zk_rollup_update
      if: (id_016__ptmumbai__operation__alpha__contents_tag == id_016__ptmumbai__operation__alpha__contents_tag::zk_rollup_update)
  id_016__ptmumbai__operation__alpha__unsigned_operation:
    seq:
    - id: id_016__ptmumbai__operation__alpha__unsigned_operation
      type: operation__shell_header
    - id: contents
      type: contents_entries
      repeat: eos
  id_016__ptmumbai__scripted__contracts:
    seq:
    - id: code
      type: code
    - id: storage
      type: storage
  id_016__ptmumbai__smart_rollup_address:
    seq:
    - id: smart_rollup_hash
      size: 20
  id_016__ptmumbai__tx_rollup_id:
    seq:
    - id: rollup_hash
      size: 20
  inbox__proof:
    seq:
    - id: level
      type: s4
    - id: message_counter
      type: n
    - id: serialized_proof
      type: serialized_proof
  increase_paid_storage:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: z
    - id: destination
      type: id_016__ptmumbai__contract_id__originated
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
      type: s4
    - id: init_state
      type: init_state
      size: len_init_state
  init_state_entries:
    seq:
    - id: init_state_elt
      size: 32
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
  kernel:
    seq:
    - id: len_kernel
      type: s4
    - id: kernel
      size: len_kernel
  message:
    seq:
    - id: message_tag
      type: u1
      enum: message_tag
    - id: batch
      type: batch
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
      type: s4
    - id: message
      type: message_0
      size: len_message
  message_entries:
    seq:
    - id: len_message_elt
      type: s4
    - id: message_elt
      size: len_message_elt
  message_path:
    seq:
    - id: message_path_entries
      type: message_path_entries
      repeat: eos
  message_path_0:
    seq:
    - id: len_message_path
      type: s4
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
      type: s4
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
      type: s4
    - id: messages
      type: messages
      size: len_messages
  messages_entries:
    seq:
    - id: message_result_hash
      size: 32
  micheline__016__ptmumbai__michelson_v1__expression:
    seq:
    - id: micheline__016__ptmumbai__michelson_v1__expression_tag
      type: u1
      enum: micheline__016__ptmumbai__michelson_v1__expression_tag
    - id: int
      type: z
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::int)
    - id: string
      type: string
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::string)
    - id: sequence
      type: sequence_0
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::sequence)
    - id: prim__no_args__no_annots
      type: id_016__ptmumbai__michelson__v1__primitives
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::prim__no_args__no_annots)
    - id: prim__no_args__some_annots
      type: prim__no_args__some_annots
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::prim__no_args__some_annots)
    - id: prim__1_arg__no_annots
      type: prim__1_arg__no_annots
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::prim__1_arg__no_annots)
    - id: prim__1_arg__some_annots
      type: prim__1_arg__some_annots
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::prim__1_arg__some_annots)
    - id: prim__2_args__no_annots
      type: prim__2_args__no_annots
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::prim__2_args__no_annots)
    - id: prim__2_args__some_annots
      type: prim__2_args__some_annots
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::prim__2_args__some_annots)
    - id: prim__generic
      type: prim__generic
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::prim__generic)
    - id: bytes
      type: bytes
      if: (micheline__016__ptmumbai__michelson_v1__expression_tag == micheline__016__ptmumbai__michelson_v1__expression_tag::bytes)
  move:
    seq:
    - id: choice
      type: n
    - id: step
      type: step
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
    - id: named
      type: named
      size: len_named
  new_state:
    seq:
    - id: new_state_entries
      type: new_state_entries
      repeat: eos
  new_state_0:
    seq:
    - id: len_new_state
      type: s4
    - id: new_state
      type: new_state
      size: len_new_state
  new_state_entries:
    seq:
    - id: new_state_elt
      size: 32
  op:
    seq:
    - id: op_entries
      type: op_entries
      repeat: eos
  op1:
    seq:
    - id: id_016__ptmumbai__inlined__endorsement
      type: id_016__ptmumbai__inlined__endorsement
  op1_0:
    seq:
    - id: len_op1
      type: s4
    - id: op1
      type: op1
      size: len_op1
  op1_1:
    seq:
    - id: id_016__ptmumbai__inlined__preendorsement
      type: id_016__ptmumbai__inlined__preendorsement
  op1_2:
    seq:
    - id: len_op1
      type: s4
    - id: op1
      type: op1_1
      size: len_op1
  op2:
    seq:
    - id: id_016__ptmumbai__inlined__endorsement
      type: id_016__ptmumbai__inlined__endorsement
  op2_0:
    seq:
    - id: len_op2
      type: s4
    - id: op2
      type: op2
      size: len_op2
  op2_1:
    seq:
    - id: id_016__ptmumbai__inlined__preendorsement
      type: id_016__ptmumbai__inlined__preendorsement
  op2_2:
    seq:
    - id: len_op2
      type: s4
    - id: op2
      type: op2_1
      size: len_op2
  op_0:
    seq:
    - id: len_op
      type: s4
    - id: op
      type: op
      size: len_op
  op_elt_field0:
    seq:
    - id: op_code
      type: s4
    - id: price
      type: price
    - id: l1_dst
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
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
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: balance
      type: id_016__ptmumbai__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: script
      type: id_016__ptmumbai__scripted__contracts
  origination_proof:
    seq:
    - id: len_origination_proof
      type: s4
    - id: origination_proof
      size: len_origination_proof
  output_proof:
    seq:
    - id: len_output_proof
      type: s4
    - id: output_proof
      size: len_output_proof
  parameters:
    seq:
    - id: entrypoint
      type: id_016__ptmumbai__entrypoint
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: value
      type: value
  parameters_ty:
    seq:
    - id: len_parameters_ty
      type: s4
    - id: parameters_ty
      size: len_parameters_ty
  payload:
    seq:
    - id: payload_entries
      type: payload_entries
      repeat: eos
  payload_0:
    seq:
    - id: len_payload
      type: s4
    - id: payload
      type: payload
      size: len_payload
  payload_entries:
    seq:
    - id: payload_elt
      size: 32
  pending_pis:
    seq:
    - id: pending_pis_entries
      type: pending_pis_entries
      repeat: eos
  pending_pis_0:
    seq:
    - id: len_pending_pis
      type: s4
    - id: pending_pis
      type: pending_pis
      size: len_pending_pis
  pending_pis_elt_field0:
    seq:
    - id: len_pending_pis_elt_field0
      type: s4
    - id: pending_pis_elt_field0
      size: len_pending_pis_elt_field0
  pending_pis_elt_field1:
    seq:
    - id: new_state
      type: new_state_0
    - id: fee
      size: 32
    - id: exit_validity
      type: u1
      enum: bool
  pending_pis_entries:
    seq:
    - id: pending_pis_elt_field0
      type: pending_pis_elt_field0
    - id: pending_pis_elt_field1
      type: pending_pis_elt_field1
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
      type: u2
    - id: level
      type: s4
    - id: round
      type: s4
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
      type: s4
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
      type: id_016__ptmumbai__michelson__v1__primitives
    - id: arg
      type: micheline__016__ptmumbai__michelson_v1__expression
  prim__1_arg__some_annots:
    seq:
    - id: prim
      type: id_016__ptmumbai__michelson__v1__primitives
    - id: arg
      type: micheline__016__ptmumbai__michelson_v1__expression
    - id: annots
      type: annots
  prim__2_args__no_annots:
    seq:
    - id: prim
      type: id_016__ptmumbai__michelson__v1__primitives
    - id: arg1
      type: micheline__016__ptmumbai__michelson_v1__expression
    - id: arg2
      type: micheline__016__ptmumbai__michelson_v1__expression
  prim__2_args__some_annots:
    seq:
    - id: prim
      type: id_016__ptmumbai__michelson__v1__primitives
    - id: arg1
      type: micheline__016__ptmumbai__michelson_v1__expression
    - id: arg2
      type: micheline__016__ptmumbai__michelson_v1__expression
    - id: annots
      type: annots
  prim__generic:
    seq:
    - id: prim
      type: id_016__ptmumbai__michelson__v1__primitives
    - id: args
      type: args_0
    - id: annots
      type: annots
  prim__no_args__some_annots:
    seq:
    - id: prim
      type: id_016__ptmumbai__michelson__v1__primitives
    - id: annots
      type: annots
  private_pis:
    seq:
    - id: private_pis_entries
      type: private_pis_entries
      repeat: eos
  private_pis_0:
    seq:
    - id: len_private_pis
      type: s4
    - id: private_pis
      type: private_pis
      size: len_private_pis
  private_pis_elt_field0:
    seq:
    - id: len_private_pis_elt_field0
      type: s4
    - id: private_pis_elt_field0
      size: len_private_pis_elt_field0
  private_pis_elt_field1:
    seq:
    - id: new_state
      type: new_state_0
    - id: fee
      size: 32
  private_pis_entries:
    seq:
    - id: private_pis_elt_field0
      type: private_pis_elt_field0
    - id: private_pis_elt_field1
      type: private_pis_elt_field1
  proof:
    seq:
    - id: len_proof
      type: s4
    - id: proof
      size: len_proof
  proof_0:
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
      type: s4
    - id: proposals
      type: proposals
      size: len_proposals
  proposals_1:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: period
      type: s4
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
    - id: bls
      size: 48
      if: (public_key_tag == public_key_tag::bls)
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
  public_parameters:
    seq:
    - id: len_public_parameters
      type: s4
    - id: public_parameters
      size: len_public_parameters
  pvm_step:
    seq:
    - id: len_pvm_step
      type: s4
    - id: pvm_step
      size: len_pvm_step
  raw_data:
    seq:
    - id: raw_data
      size-eos: true
  raw_data_0:
    seq:
    - id: len_raw_data
      type: u2
    - id: raw_data
      type: raw_data
      size: len_raw_data
  refutation:
    seq:
    - id: refutation_tag
      type: u1
      enum: refutation_tag
    - id: start
      type: start
      if: (refutation_tag == refutation_tag::start)
    - id: move
      type: move
      if: (refutation_tag == refutation_tag::move)
  register_global_constant:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: value
      type: value
  reveal:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
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
      type: raw_data_0
      if: (reveal_proof_tag == reveal_proof_tag::raw__data__proof)
    - id: dal__page__proof
      type: dal__page__proof
      if: (reveal_proof_tag == reveal_proof_tag::dal__page__proof)
  seed_nonce_revelation:
    seq:
    - id: level
      type: s4
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
      type: s4
    - id: sequence
      type: sequence
      size: len_sequence
  sequence_entries:
    seq:
    - id: sequence_elt
      type: micheline__016__ptmumbai__michelson_v1__expression
  serialized_proof:
    seq:
    - id: len_serialized_proof
      type: s4
    - id: serialized_proof
      size: len_serialized_proof
  set_deposits_limit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
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
      type: id_016__ptmumbai__mutez
      if: (limit_tag == bool::true)
  slot_header:
    seq:
    - id: level
      type: s4
    - id: index
      type: u1
    - id: commitment
      size: 48
    - id: commitment_proof
      size: 48
  smart_rollup_add_messages:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: message
      type: message_1
  smart_rollup_cement:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__smart_rollup_address
      doc: ! >-
        A smart rollup address: A smart rollup is identified by a base58 address starting
        with sr1
    - id: commitment
      size: 32
  smart_rollup_execute_outbox_message:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__smart_rollup_address
      doc: ! >-
        A smart rollup address: A smart rollup is identified by a base58 address starting
        with sr1
    - id: cemented_commitment
      size: 32
    - id: output_proof
      type: output_proof
  smart_rollup_originate:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pvm_kind
      type: u1
      enum: pvm_kind
    - id: kernel
      type: kernel
    - id: origination_proof
      type: origination_proof
    - id: parameters_ty
      type: parameters_ty
  smart_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__smart_rollup_address
      doc: ! >-
        A smart rollup address: A smart rollup is identified by a base58 address starting
        with sr1
    - id: commitment
      type: commitment_0
  smart_rollup_recover_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      size: 20
    - id: staker
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  smart_rollup_refute:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__smart_rollup_address
      doc: ! >-
        A smart rollup address: A smart rollup is identified by a base58 address starting
        with sr1
    - id: opponent
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: refutation
      type: refutation
  smart_rollup_timeout:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__smart_rollup_address
      doc: ! >-
        A smart rollup address: A smart rollup is identified by a base58 address starting
        with sr1
    - id: stakers
      type: stakers
  solution:
    seq:
    - id: solution_field0
      size: 100
    - id: solution_field1
      size: 100
  some:
    seq:
    - id: contents
      type: micheline__016__ptmumbai__michelson_v1__expression
    - id: ty
      type: micheline__016__ptmumbai__michelson_v1__expression
    - id: ticketer
      type: id_016__ptmumbai__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
  stakers:
    seq:
    - id: alice
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: bob
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  start:
    seq:
    - id: player_commitment_hash
      size: 32
    - id: opponent_commitment_hash
      size: 32
  step:
    seq:
    - id: step_tag
      type: u1
      enum: step_tag
    - id: dissection
      type: dissection_0
      if: (step_tag == step_tag::dissection)
    - id: proof
      type: proof_0
      if: (step_tag == step_tag::proof)
  storage:
    seq:
    - id: len_storage
      type: s4
    - id: storage
      size: len_storage
  string:
    seq:
    - id: len_string
      type: s4
    - id: string
      size: len_string
  ticket_contents:
    seq:
    - id: len_ticket_contents
      type: s4
    - id: ticket_contents
      size: len_ticket_contents
  ticket_ty:
    seq:
    - id: len_ticket_ty
      type: s4
    - id: ticket_ty
      size: len_ticket_ty
  tickets_info:
    seq:
    - id: tickets_info_entries
      type: tickets_info_entries
      repeat: eos
  tickets_info_0:
    seq:
    - id: len_tickets_info
      type: s4
    - id: tickets_info
      type: tickets_info
      size: len_tickets_info
  tickets_info_entries:
    seq:
    - id: contents
      type: contents
    - id: ty
      type: ty
    - id: ticketer
      type: id_016__ptmumbai__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: amount
      type: amount
    - id: claimer
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
  transaction:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: id_016__ptmumbai__mutez
    - id: destination
      type: id_016__ptmumbai__contract_id
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
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: ticket_contents
      type: ticket_contents
    - id: ticket_ty
      type: ticket_ty
    - id: ticket_ticketer
      type: id_016__ptmumbai__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: ticket_amount
      type: n
    - id: destination
      type: id_016__ptmumbai__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: entrypoint
      type: entrypoint
  tx_rollup_commit:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: commitment
      type: commitment
  tx_rollup_dispatch_tickets:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: tx_rollup
      type: id_016__ptmumbai__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: level
      type: s4
    - id: context_hash
      size: 32
    - id: message_index
      type: s4
    - id: message_result_path
      type: message_result_path_0
    - id: tickets_info
      type: tickets_info_0
  tx_rollup_finalize_commitment:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_origination:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
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
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: level
      type: s4
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
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_return_bond:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
  tx_rollup_submit_batch:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: rollup
      type: id_016__ptmumbai__tx_rollup_id
      doc: ! >-
        A tx rollup handle: A tx rollup notation as given to an RPC or inside scripts,
        is a base58 tx rollup hash
    - id: content
      type: content
    - id: burn_limit_tag
      type: u1
      enum: bool
    - id: burn_limit
      type: id_016__ptmumbai__mutez
      if: (burn_limit_tag == bool::true)
  ty:
    seq:
    - id: len_ty
      type: s4
    - id: ty
      size: len_ty
  update:
    seq:
    - id: pending_pis
      type: pending_pis_0
    - id: private_pis
      type: private_pis_0
    - id: fee_pi
      type: new_state_0
    - id: proof
      type: proof
  update_consensus_key:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: pk
      type: public_key
      doc: A Ed25519, Secp256k1, or P256 public key
  value:
    seq:
    - id: len_value
      type: s4
    - id: value
      size: len_value
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
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
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
      type: s4
  zk_rollup_publish:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
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
  zk_rollup_update:
    seq:
    - id: source
      type: public_key_hash
      doc: A Ed25519, Secp256k1, P256, or BLS public key hash
    - id: fee
      type: id_016__ptmumbai__mutez
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: zk_rollup
      size: 20
    - id: update
      type: update
enums:
  amount_tag:
    0: case__0
    1: case__1
    2: case__2
    3: case__3
  bool:
    0: false
    255: true
  circuits_info_elt_field1_tag:
    0: public
    1: private
    2: fee
  id_016__ptmumbai__contract_id__originated_tag:
    1: originated
  id_016__ptmumbai__contract_id_tag:
    0: implicit
    1: originated
  id_016__ptmumbai__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    5: deposit
    255: named
  id_016__ptmumbai__inlined__endorsement_mempool__contents_tag:
    21: endorsement
  id_016__ptmumbai__inlined__preendorsement__contents_tag:
    20: preendorsement
  id_016__ptmumbai__michelson__v1__primitives:
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
    155:
      id: bytes_0
      doc: BYTES
    156:
      id: nat_0
      doc: NAT
  id_016__ptmumbai__operation__alpha__contents_tag:
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
    22: dal_attestation
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
    200: smart_rollup_originate
    201: smart_rollup_add_messages
    202: smart_rollup_cement
    203: smart_rollup_publish
    204: smart_rollup_refute
    205: smart_rollup_timeout
    206: smart_rollup_execute_outbox_message
    207: smart_rollup_recover_bond
    230: dal_publish_slot_header
    250: zk_rollup_origination
    251: zk_rollup_publish
    252: zk_rollup_update
  input_proof_tag:
    0: inbox__proof
    1: reveal__proof
    2: first__input
  message_tag:
    0: batch
    1: deposit
  micheline__016__ptmumbai__michelson_v1__expression_tag:
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
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
    3: bls
  pvm_kind:
    0: arith
    1: wasm_2_0_0
  refutation_tag:
    0: start
    1: move
  reveal_proof_tag:
    0: raw__data__proof
    1: metadata__proof
    2: dal__page__proof
  step_tag:
    0: dissection
    1: proof
seq:
- id: id_016__ptmumbai__operation__alpha__unsigned_operation
  type: id_016__ptmumbai__operation__alpha__unsigned_operation
