meta:
  id: id_011__pthangz2__operation__internal
  endian: be
doc: ! 'Encoding id: 011-PtHangz2.operation.internal'
types:
  bytes_dyn_uint30:
    seq:
    - id: len_bytes_dyn_uint30
      type: u4
      valid:
        max: 1073741823
    - id: bytes_dyn_uint30
      size: len_bytes_dyn_uint30
  delegation:
    seq:
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, or P256 public key hash
  id_011__pthangz2__contract_id:
    seq:
    - id: id_011__pthangz2__contract_id_tag
      type: u1
      enum: id_011__pthangz2__contract_id_tag
    - id: implicit
      type: public_key_hash
      if: (id_011__pthangz2__contract_id_tag == id_011__pthangz2__contract_id_tag::implicit)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: originated
      type: originated
      if: (id_011__pthangz2__contract_id_tag == id_011__pthangz2__contract_id_tag::originated)
  id_011__pthangz2__entrypoint:
    seq:
    - id: id_011__pthangz2__entrypoint_tag
      type: u1
      enum: id_011__pthangz2__entrypoint_tag
    - id: named
      type: named_0
      if: (id_011__pthangz2__entrypoint_tag == id_011__pthangz2__entrypoint_tag::named)
  id_011__pthangz2__mutez:
    seq:
    - id: id_011__pthangz2__mutez
      type: n
  id_011__pthangz2__operation__alpha__internal_operation:
    seq:
    - id: source
      type: id_011__pthangz2__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: nonce
      type: u2
    - id: id_011__pthangz2__operation__alpha__internal_operation_tag
      type: u1
      enum: id_011__pthangz2__operation__alpha__internal_operation_tag
    - id: reveal
      type: public_key
      if: (id_011__pthangz2__operation__alpha__internal_operation_tag == id_011__pthangz2__operation__alpha__internal_operation_tag::reveal)
      doc: A Ed25519, Secp256k1, or P256 public key
    - id: transaction
      type: transaction
      if: (id_011__pthangz2__operation__alpha__internal_operation_tag == id_011__pthangz2__operation__alpha__internal_operation_tag::transaction)
    - id: origination
      type: origination
      if: (id_011__pthangz2__operation__alpha__internal_operation_tag == id_011__pthangz2__operation__alpha__internal_operation_tag::origination)
    - id: delegation
      type: delegation
      if: (id_011__pthangz2__operation__alpha__internal_operation_tag == id_011__pthangz2__operation__alpha__internal_operation_tag::delegation)
    - id: register_global_constant
      type: bytes_dyn_uint30
      if: (id_011__pthangz2__operation__alpha__internal_operation_tag == id_011__pthangz2__operation__alpha__internal_operation_tag::register_global_constant)
  id_011__pthangz2__scripted__contracts:
    seq:
    - id: code
      type: bytes_dyn_uint30
    - id: storage
      type: bytes_dyn_uint30
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
      type: id_011__pthangz2__mutez
    - id: delegate_tag
      type: u1
      enum: bool
    - id: delegate
      type: public_key_hash
      if: (delegate_tag == bool::true)
      doc: A Ed25519, Secp256k1, or P256 public key hash
    - id: script
      type: id_011__pthangz2__scripted__contracts
  parameters:
    seq:
    - id: entrypoint
      type: id_011__pthangz2__entrypoint
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: value
      type: bytes_dyn_uint30
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
  transaction:
    seq:
    - id: amount
      type: id_011__pthangz2__mutez
    - id: destination
      type: id_011__pthangz2__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: parameters_tag
      type: u1
      enum: bool
    - id: parameters
      type: parameters
      if: (parameters_tag == bool::true)
enums:
  bool:
    0: false
    255: true
  id_011__pthangz2__contract_id_tag:
    0: implicit
    1: originated
  id_011__pthangz2__entrypoint_tag:
    0: default
    1: root
    2: do
    3: set_delegate
    4: remove_delegate
    255: named
  id_011__pthangz2__operation__alpha__internal_operation_tag:
    0: reveal
    1: transaction
    2: origination
    3: delegation
    4: register_global_constant
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
  public_key_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: id_011__pthangz2__operation__alpha__internal_operation
  type: id_011__pthangz2__operation__alpha__internal_operation
