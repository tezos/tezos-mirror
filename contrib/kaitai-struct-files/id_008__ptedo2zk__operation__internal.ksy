meta:
  id: id_008__ptedo2zk__operation__internal
  endian: be
types:
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  id_008__ptedo2zk__contract_id:
    doc: ! >-
      A contract handle: A contract notation as given to an RPC or inside scripts.
      Can be a base58 implicit contract hash or a base58 originated contract hash.
    seq:
    - id: id_008__ptedo2zk__contract_id_tag
      type: u1
      enum: id_008__ptedo2zk__contract_id_tag
    - id: id_008__ptedo2zk__contract_id_implicit
      type: public_key_hash
      if: (id_008__ptedo2zk__contract_id_tag == id_008__ptedo2zk__contract_id_tag::implicit)
    - id: id_008__ptedo2zk__contract_id_originated
      type: id_008__ptedo2zk__contract_id_originated
      if: (id_008__ptedo2zk__contract_id_tag == id_008__ptedo2zk__contract_id_tag::originated)
  id_008__ptedo2zk__contract_id_originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  id_008__ptedo2zk__entrypoint:
    doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    seq:
    - id: id_008__ptedo2zk__entrypoint_tag
      type: u1
      enum: id_008__ptedo2zk__entrypoint_tag
    - id: id_008__ptedo2zk__entrypoint_named
      type: id_008__ptedo2zk__entrypoint_named
      if: (id_008__ptedo2zk__entrypoint_tag == id_008__ptedo2zk__entrypoint_tag::named)
  id_008__ptedo2zk__entrypoint_named:
    seq:
    - id: len_named
      type: u1
    - id: named
      size: len_named
      size-eos: true
  id_008__ptedo2zk__operation__alpha__internal_operation:
    seq:
    - id: source
      type: id_008__ptedo2zk__contract_id
    - id: nonce
      type: u2
    - id: id_008__ptedo2zk__operation__alpha__internal_operation_tag
      type: u1
      enum: id_008__ptedo2zk__operation__alpha__internal_operation_tag
    - id: id_008__ptedo2zk__operation__alpha__internal_operation_reveal
      type: public_key
      if: (id_008__ptedo2zk__operation__alpha__internal_operation_tag == id_008__ptedo2zk__operation__alpha__internal_operation_tag::reveal)
    - id: id_008__ptedo2zk__operation__alpha__internal_operation_transaction
      type: id_008__ptedo2zk__operation__alpha__internal_operation_transaction
      if: (id_008__ptedo2zk__operation__alpha__internal_operation_tag == id_008__ptedo2zk__operation__alpha__internal_operation_tag::transaction)
    - id: id_008__ptedo2zk__operation__alpha__internal_operation_origination
      type: id_008__ptedo2zk__operation__alpha__internal_operation_origination
      if: (id_008__ptedo2zk__operation__alpha__internal_operation_tag == id_008__ptedo2zk__operation__alpha__internal_operation_tag::origination)
    - id: id_008__ptedo2zk__operation__alpha__internal_operation_delegation
      type: id_008__ptedo2zk__operation__alpha__internal_operation_delegation
      if: (id_008__ptedo2zk__operation__alpha__internal_operation_tag == id_008__ptedo2zk__operation__alpha__internal_operation_tag::delegation)
  id_008__ptedo2zk__operation__alpha__internal_operation_delegation:
    seq:
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
  id_008__ptedo2zk__operation__alpha__internal_operation_origination:
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
      type: id_008__ptedo2zk__scripted__contracts
  id_008__ptedo2zk__operation__alpha__internal_operation_transaction:
    seq:
    - id: amount
      type: n
    - id: destination
      type: id_008__ptedo2zk__contract_id
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
  id_008__ptedo2zk__scripted__contracts:
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
  parameters:
    seq:
    - id: entrypoint
      type: id_008__ptedo2zk__entrypoint
    - id: value
      type: value
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
  id_008__ptedo2zk__contract_id_tag:
    0: implicit
    1: originated
  id_008__ptedo2zk__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    255: named
  id_008__ptedo2zk__operation__alpha__internal_operation_tag:
    0: reveal
    1: transaction
    2: origination
    3: delegation
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: id_008__ptedo2zk__operation__alpha__internal_operation
  type: id_008__ptedo2zk__operation__alpha__internal_operation
