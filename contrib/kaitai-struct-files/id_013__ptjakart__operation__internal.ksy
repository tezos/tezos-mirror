meta:
  id: id_013__ptjakart__operation__internal
  endian: be
doc: ! 'Encoding id: 013-PtJakart.operation.internal'
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
  id_013__ptjakart__apply_results__alpha__internal_operation_result:
    seq:
    - id: source
      type: id_013__ptjakart__contract_id
      doc: ! >-
        A contract handle: A contract notation as given to an RPC or inside scripts.
        Can be a base58 implicit contract hash or a base58 originated contract hash.
    - id: nonce
      type: u2
    - id: id_013__ptjakart__apply_results__alpha__internal_operation_result_tag
      type: u1
      enum: id_013__ptjakart__apply_results__alpha__internal_operation_result_tag
    - id: transaction
      type: transaction
      if: (id_013__ptjakart__apply_results__alpha__internal_operation_result_tag ==
        id_013__ptjakart__apply_results__alpha__internal_operation_result_tag::transaction)
    - id: origination
      type: origination
      if: (id_013__ptjakart__apply_results__alpha__internal_operation_result_tag ==
        id_013__ptjakart__apply_results__alpha__internal_operation_result_tag::origination)
    - id: delegation
      type: delegation
      if: (id_013__ptjakart__apply_results__alpha__internal_operation_result_tag ==
        id_013__ptjakart__apply_results__alpha__internal_operation_result_tag::delegation)
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
  id_013__ptjakart__mutez:
    seq:
    - id: id_013__ptjakart__mutez
      type: n
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
  parameters:
    seq:
    - id: entrypoint
      type: id_013__ptjakart__entrypoint
      doc: ! 'entrypoint: Named entrypoint to a Michelson smart contract'
    - id: value
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
  transaction:
    seq:
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
enums:
  bool:
    0: false
    255: true
  id_013__ptjakart__apply_results__alpha__internal_operation_result_tag:
    1: transaction
    2: origination
    3: delegation
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
  id_013__ptjakart__transaction_destination_tag:
    0: implicit
    1: originated
    2: tx_rollup
  public_key_hash_tag:
    0: ed25519
    1: secp256k1
    2: p256
seq:
- id: id_013__ptjakart__apply_results__alpha__internal_operation_result
  type: id_013__ptjakart__apply_results__alpha__internal_operation_result
