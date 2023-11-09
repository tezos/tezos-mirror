meta:
  id: id_009__psfloren__operation__unsigned
  endian: be
types:
  arbitrary:
    seq:
    - id: len_arbitrary
      type: s4
    - id: arbitrary
      size: len_arbitrary
  bh1:
    seq:
    - id: len_bh1
      type: s4
    - id: bh1
      type: id_009__psfloren__block_header__alpha__full_header
      size: len_bh1
  bh2:
    seq:
    - id: len_bh2
      type: s4
    - id: bh2
      type: id_009__psfloren__block_header__alpha__full_header
      size: len_bh2
  block_header__shell:
    doc: ! >-
      Shell header: Block header's shell-related content. It contains information
      such as the block level, its predecessor and timestamp.
    seq:
    - id: level
      type: s4
    - id: proto
      type: u1
    - id: predecessor
      size: 32
    - id: timestamp
      type: s8
      doc: ! 'A timestamp as seen by the protocol: second-level precision, epoch based.'
    - id: validation_pass
      type: u1
    - id: operations_hash
      size: 32
    - id: fitness
      type: fitness
    - id: context
      size: 32
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  contents_entries:
    seq:
    - id: id_009__psfloren__operation__alpha__contents
      type: id_009__psfloren__operation__alpha__contents
  endorsement:
    seq:
    - id: len_endorsement
      type: s4
    - id: endorsement
      type: id_009__psfloren__inlined__endorsement
      size: len_endorsement
  fitness:
    doc: ! >-
      Block fitness: The fitness, or score, of a block, that allow the Tezos to decide
      which chain is the best. A fitness value is a list of byte sequences. They are
      compared as follows: shortest lists are smaller; lists of the same length are
      compared according to the lexicographical order.
    seq:
    - id: len_fitness
      type: s4
    - id: fitness
      type: fitness_entries
      size: len_fitness
      repeat: eos
  fitness__elem:
    seq:
    - id: len_fitness__elem
      type: s4
    - id: fitness__elem
      size: len_fitness__elem
  fitness_entries:
    seq:
    - id: fitness__elem
      type: fitness__elem
  id_009__psfloren__block_header__alpha__full_header:
    seq:
    - id: block_header__shell
      type: block_header__shell
    - id: id_009__psfloren__block_header__alpha__signed_contents
      type: id_009__psfloren__block_header__alpha__signed_contents
  id_009__psfloren__block_header__alpha__signed_contents:
    seq:
    - id: id_009__psfloren__block_header__alpha__unsigned_contents
      type: id_009__psfloren__block_header__alpha__unsigned_contents
    - id: signature
      size: 64
  id_009__psfloren__block_header__alpha__unsigned_contents:
    seq:
    - id: priority
      type: u2
    - id: proof_of_work_nonce
      size: 8
    - id: seed_nonce_hash_tag
      type: u1
      enum: bool
    - id: seed_nonce_hash
      size: 32
      if: (seed_nonce_hash_tag == bool::true)
  id_009__psfloren__contract_id:
    doc: ! >-
      A contract handle: A contract notation as given to an RPC or inside scripts.
      Can be a base58 implicit contract hash or a base58 originated contract hash.
    seq:
    - id: id_009__psfloren__contract_id_tag
      type: u1
      enum: id_009__psfloren__contract_id_tag
    - id: id_009__psfloren__contract_id_implicit
      type: public_key_hash
      if: (id_009__psfloren__contract_id_tag == id_009__psfloren__contract_id_tag::implicit)
    - id: id_009__psfloren__contract_id_originated
      type: id_009__psfloren__contract_id_originated
      if: (id_009__psfloren__contract_id_tag == id_009__psfloren__contract_id_tag::originated)
  id_009__psfloren__contract_id_originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  id_009__psfloren__entrypoint:
    doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    seq:
    - id: id_009__psfloren__entrypoint_tag
      type: u1
      enum: id_009__psfloren__entrypoint_tag
    - id: id_009__psfloren__entrypoint_named
      type: id_009__psfloren__entrypoint_named
      if: (id_009__psfloren__entrypoint_tag == id_009__psfloren__entrypoint_tag::named)
  id_009__psfloren__entrypoint_named:
    seq:
    - id: len_named
      type: u1
    - id: named
      size: len_named
      size-eos: true
  id_009__psfloren__inlined__endorsement:
    seq:
    - id: branch
      size: 32
      doc: An operation's shell header.
    - id: operations
      type: id_009__psfloren__inlined__endorsement__contents
    - id: signature_tag
      type: u1
      enum: bool
    - id: signature
      size: 64
      if: (signature_tag == bool::true)
  id_009__psfloren__inlined__endorsement__contents:
    seq:
    - id: id_009__psfloren__inlined__endorsement__contents_tag
      type: u1
      enum: id_009__psfloren__inlined__endorsement__contents_tag
    - id: id_009__psfloren__inlined__endorsement__contents_endorsement
      type: s4
      if: (id_009__psfloren__inlined__endorsement__contents_tag == id_009__psfloren__inlined__endorsement__contents_tag::endorsement)
  id_009__psfloren__operation__alpha__contents:
    seq:
    - id: id_009__psfloren__operation__alpha__contents_tag
      type: u1
      enum: id_009__psfloren__operation__alpha__contents_tag
    - id: id_009__psfloren__operation__alpha__contents_endorsement
      type: s4
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::endorsement)
    - id: id_009__psfloren__operation__alpha__contents_seed_nonce_revelation
      type: id_009__psfloren__operation__alpha__contents_seed_nonce_revelation
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::seed_nonce_revelation)
    - id: id_009__psfloren__operation__alpha__contents_endorsement_with_slot
      type: id_009__psfloren__operation__alpha__contents_endorsement_with_slot
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::endorsement_with_slot)
    - id: id_009__psfloren__operation__alpha__contents_double_endorsement_evidence
      type: id_009__psfloren__operation__alpha__contents_double_endorsement_evidence
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::double_endorsement_evidence)
    - id: id_009__psfloren__operation__alpha__contents_double_baking_evidence
      type: id_009__psfloren__operation__alpha__contents_double_baking_evidence
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::double_baking_evidence)
    - id: id_009__psfloren__operation__alpha__contents_activate_account
      type: id_009__psfloren__operation__alpha__contents_activate_account
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::activate_account)
    - id: id_009__psfloren__operation__alpha__contents_proposals
      type: id_009__psfloren__operation__alpha__contents_proposals
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::proposals)
    - id: id_009__psfloren__operation__alpha__contents_ballot
      type: id_009__psfloren__operation__alpha__contents_ballot
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::ballot)
    - id: id_009__psfloren__operation__alpha__contents_reveal
      type: id_009__psfloren__operation__alpha__contents_reveal
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::reveal)
    - id: id_009__psfloren__operation__alpha__contents_transaction
      type: id_009__psfloren__operation__alpha__contents_transaction
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::transaction)
    - id: id_009__psfloren__operation__alpha__contents_origination
      type: id_009__psfloren__operation__alpha__contents_origination
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::origination)
    - id: id_009__psfloren__operation__alpha__contents_delegation
      type: id_009__psfloren__operation__alpha__contents_delegation
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::delegation)
    - id: id_009__psfloren__operation__alpha__contents_failing_noop
      type: arbitrary
      if: (id_009__psfloren__operation__alpha__contents_tag == id_009__psfloren__operation__alpha__contents_tag::failing_noop)
  id_009__psfloren__operation__alpha__contents_activate_account:
    seq:
    - id: pkh
      size: 20
    - id: secret
      size: 20
  id_009__psfloren__operation__alpha__contents_ballot:
    seq:
    - id: source
      type: public_key_hash
    - id: period
      type: s4
    - id: proposal
      size: 32
    - id: ballot
      type: s1
  id_009__psfloren__operation__alpha__contents_delegation:
    seq:
    - id: source
      type: public_key_hash
    - id: fee
      type: n
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
  id_009__psfloren__operation__alpha__contents_double_baking_evidence:
    seq:
    - id: bh1
      type: bh1
    - id: bh2
      type: bh2
  id_009__psfloren__operation__alpha__contents_double_endorsement_evidence:
    seq:
    - id: op1
      type: op1
    - id: op2
      type: op2
    - id: slot
      type: u2
  id_009__psfloren__operation__alpha__contents_endorsement_with_slot:
    seq:
    - id: endorsement
      type: endorsement
    - id: slot
      type: u2
  id_009__psfloren__operation__alpha__contents_origination:
    seq:
    - id: source
      type: public_key_hash
    - id: fee
      type: n
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: balance
      type: n
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
    - id: script
      type: id_009__psfloren__scripted__contracts
  id_009__psfloren__operation__alpha__contents_proposals:
    seq:
    - id: source
      type: public_key_hash
    - id: period
      type: s4
    - id: proposals
      type: proposals
  id_009__psfloren__operation__alpha__contents_reveal:
    seq:
    - id: source
      type: public_key_hash
    - id: fee
      type: n
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: public_key
      type: public_key
  id_009__psfloren__operation__alpha__contents_seed_nonce_revelation:
    seq:
    - id: level
      type: s4
    - id: nonce
      size: 32
  id_009__psfloren__operation__alpha__contents_transaction:
    seq:
    - id: source
      type: public_key_hash
    - id: fee
      type: n
    - id: counter
      type: n
    - id: gas_limit
      type: n
    - id: storage_limit
      type: n
    - id: amount
      type: n
    - id: destination
      type: id_009__psfloren__contract_id
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
  id_009__psfloren__operation__alpha__unsigned_operation:
    seq:
    - id: branch
      size: 32
      doc: An operation's shell header.
    - id: contents
      type: contents_entries
      repeat: eos
  id_009__psfloren__scripted__contracts:
    seq:
    - id: code
      type: code
    - id: storage
      type: storage
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
  op1:
    seq:
    - id: len_op1
      type: s4
    - id: op1
      type: id_009__psfloren__inlined__endorsement
      size: len_op1
  op2:
    seq:
    - id: len_op2
      type: s4
    - id: op2
      type: id_009__psfloren__inlined__endorsement
      size: len_op2
  parameters:
    seq:
    - id: entrypoint
      type: id_009__psfloren__entrypoint
    - id: value
      type: value
  proposals:
    seq:
    - id: len_proposals
      type: s4
    - id: proposals
      type: proposals_entries
      size: len_proposals
      repeat: eos
  proposals_entries:
    seq:
    - id: protocol_hash
      size: 32
  public_key:
    doc: A Ed25519, Secp256k1, or P256 public key
    seq:
    - id: public_key_tag
      type: u1
      enum: public_key_tag
    - id: public_key_ed25519
      size: 32
      if: (public_key_tag == public_key_tag::ed25519)
    - id: public_key_secp256k1
      size: 33
      if: (public_key_tag == public_key_tag::secp256k1)
    - id: public_key_p256
      size: 33
      if: (public_key_tag == public_key_tag::p256)
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
  storage:
    seq:
    - id: len_storage
      type: s4
    - id: storage
      size: len_storage
  value:
    seq:
    - id: len_value
      type: s4
    - id: value
      size: len_value
enums:
  bool:
    0: false
    255: true
  id_009__psfloren__contract_id_tag:
    0: implicit
    1: originated
  id_009__psfloren__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    255: named
  id_009__psfloren__inlined__endorsement__contents_tag:
    0: endorsement
  id_009__psfloren__operation__alpha__contents_tag:
    0: endorsement
    1: seed_nonce_revelation
    2: double_endorsement_evidence
    3: double_baking_evidence
    4: activate_account
    5: proposals
    6: ballot
    10: endorsement_with_slot
    17: failing_noop
    107: reveal
    108: transaction
    109: origination
    110: delegation
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: id_009__psfloren__operation__alpha__unsigned_operation
  type: id_009__psfloren__operation__alpha__unsigned_operation
