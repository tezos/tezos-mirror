meta:
  id: id_012__psithaca__operation__internal
  endian: be
types:
  code:
    seq:
    - id: len_code
      type: s4
    - id: code
      size: len_code
  delegation:
    seq:
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
  id_012__psithaca__contract_id:
    doc: ! >-
      A contract handle: A contract notation as given to an RPC or inside scripts.
      Can be a base58 implicit contract hash or a base58 originated contract hash.
    seq:
    - id: id_012__psithaca__contract_id_tag
      type: u1
      enum: id_012__psithaca__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_012__psithaca__contract_id_tag == id_012__psithaca__contract_id_tag::implicit)
    - id: originated
      type: originated
      if: (id_012__psithaca__contract_id_tag == id_012__psithaca__contract_id_tag::originated)
  id_012__psithaca__entrypoint:
    doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    seq:
    - id: id_012__psithaca__entrypoint_tag
      type: u1
      enum: id_012__psithaca__entrypoint_tag
    - id: named
      type: named
      if: (id_012__psithaca__entrypoint_tag == id_012__psithaca__entrypoint_tag::named)
  id_012__psithaca__operation__alpha__internal_operation:
    seq:
    - id: source
      type: id_012__psithaca__contract_id
    - id: nonce
      type: u2
    - id: id_012__psithaca__operation__alpha__internal_operation_tag
      type: u1
      enum: id_012__psithaca__operation__alpha__internal_operation_tag
    - id: reveal
      type: public_key
      if: (id_012__psithaca__operation__alpha__internal_operation_tag == id_012__psithaca__operation__alpha__internal_operation_tag::reveal)
    - id: transaction
      type: transaction
      if: (id_012__psithaca__operation__alpha__internal_operation_tag == id_012__psithaca__operation__alpha__internal_operation_tag::transaction)
    - id: origination
      type: origination
      if: (id_012__psithaca__operation__alpha__internal_operation_tag == id_012__psithaca__operation__alpha__internal_operation_tag::origination)
    - id: delegation
      type: delegation
      if: (id_012__psithaca__operation__alpha__internal_operation_tag == id_012__psithaca__operation__alpha__internal_operation_tag::delegation)
    - id: register_global_constant
      type: value
      if: (id_012__psithaca__operation__alpha__internal_operation_tag == id_012__psithaca__operation__alpha__internal_operation_tag::register_global_constant)
    - id: set_deposits_limit
      type: set_deposits_limit
      if: (id_012__psithaca__operation__alpha__internal_operation_tag == id_012__psithaca__operation__alpha__internal_operation_tag::set_deposits_limit)
  id_012__psithaca__scripted__contracts:
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
  named:
    seq:
    - id: len_named
      type: u1
    - id: named
      size: len_named
      size-eos: true
  originated:
    seq:
    - id: contract_hash
      size: 20
    - id: originated_padding
      size: 1
      doc: This field is for padding, ignore
  origination:
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
      type: id_012__psithaca__scripted__contracts
  parameters:
    seq:
    - id: entrypoint
      type: id_012__psithaca__entrypoint
    - id: value
      type: value
  public_key:
    doc: A Ed25519, Secp256k1, or P256 public key
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
    doc: A Ed25519, Secp256k1, or P256 public key hash
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
  set_deposits_limit:
    seq:
    - id: limit_tag
      type: u1
      enum: bool
    - id: limit
      type: n
      if: (limit_tag == bool::true)
  storage:
    seq:
    - id: len_storage
      type: s4
    - id: storage
      size: len_storage
  transaction:
    seq:
    - id: amount
      type: n
    - id: destination
      type: id_012__psithaca__contract_id
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
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
  id_012__psithaca__contract_id_tag:
    0: implicit
    1: originated
  id_012__psithaca__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    255: named
  id_012__psithaca__operation__alpha__internal_operation_tag:
    0: reveal
    1: transaction
    2: origination
    3: delegation
    4: register_global_constant
    5: set_deposits_limit
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: id_012__psithaca__operation__alpha__internal_operation
  type: id_012__psithaca__operation__alpha__internal_operation
