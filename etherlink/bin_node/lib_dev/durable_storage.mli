(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Typed access layer for the EVM node durable storage.

    All durable storage access MUST go through this module.
    The [path] GADT ensures type-safe read/write/delete operations.
    [Raw_path] is provided as an escape hatch for paths not yet
    modeled in the GADT. New code should add typed constructors
    instead of using [Raw_path]. *)

(** Decoded form of the [/evm/world_state/accounts/<addr>/info] record:
    balance, nonce, and code hash for an EVM account. *)
module EVM_account_info : sig
  type t = {
    balance : Ethereum_types.quantity;
    nonce : Ethereum_types.quantity;
    code_hash : Ethereum_types.hash;
  }

  val encode : t -> bytes

  val decode_opt : bytes -> t option

  val decode : bytes -> t tzresult
end

(** {2 Typed path GADT}

    A path carries two phantom parameters: the decoded value type ['a]
    and a capability row ['cap] made of the polymorphic-variant tags
    [`Read] and [`Write]. The aliases {!rw} (read+write) and {!ro}
    (read-only) name the two closed rows. [Raw_path] is the escape hatch
    that decodes to raw bytes; new code should add typed constructors
    with the precise capability row instead. *)

(** Phantom capability markers for [path]: [rw] is read+write+delete, [ro] is
    read-only. *)
type rw = [`Read | `Write | `Delete]

type ro = [`Read]

type read_delete = [`Read | `Delete]

type ('a, 'cap) path =
  | Raw_path : string -> (bytes, rw) path
  | Chain_id : (L2_types.chain_id, ro) path
  | Michelson_runtime_chain_id : (L2_types.chain_id, ro) path
  | Kernel_version : (string, ro) path
  | Kernel_root_hash : (Ethereum_types.hex, ro) path
  | Multichain_flag : (unit, ro) path
  | Sequencer_key : (Signature.Public_key.t, ro) path
  | Chain_config_family :
      L2_types.chain_id
      -> (L2_types.ex_chain_family, ro) path
  | Tezosx_feature_flag : Tezosx.runtime -> (unit, ro) path
  | Michelson_runtime_sunrise_level : (Ethereum_types.quantity, ro) path
  | Current_block_number :
      _ L2_types.chain_family
      -> (Ethereum_types.quantity, ro) path
  | Current_block_hash :
      _ L2_types.chain_family
      -> (Ethereum_types.block_hash, ro) path
  | Evm_node_flag : (unit, rw) path
  | Blueprint_chunk : {
      blueprint_number : Z.t;
      chunk_index : int;
    }
      -> (bytes, rw) path
  | Blueprint_nb_chunks : Z.t -> (int, rw) path
  | Blueprint_generation : Z.t -> (Ethereum_types.quantity, rw) path
  | Single_tx_input : (Rlp.item, rw) path
  | Assemble_block_input : (Rlp.item, rw) path
  | Current_block :
      _ L2_types.chain_family
      -> (Ethereum_types.legacy_transaction_object L2_types.block, ro) path
  | Block_by_hash :
      _ L2_types.chain_family * Ethereum_types.block_hash
      -> ( Ethereum_types.legacy_transaction_object L2_types.block,
           read_delete )
         path
  | Block_index :
      _ L2_types.chain_family * Durable_storage_path.Block.number
      -> (unit, [`Delete]) path
  | Tezosx_tezos_current_block :
      (Ethereum_types.legacy_transaction_object L2_types.block, ro) path
  | Current_receipts : (Transaction_receipt.t, ro) path
  | Backlog : (int64, ro) path
  | Minimum_base_fee_per_gas : (Z.t, ro) path
  | Da_fee_per_byte : (Ethereum_types.quantity, ro) path
  | Maximum_gas_per_transaction : (Ethereum_types.quantity, ro) path
  | Michelson_to_evm_gas_multiplier : (int64, ro) path
  | Sequencer_pool_address : (Ethereum_types.address, ro) path
  | Evm_legacy_account_balance :
      Ethereum_types.address
      -> (Ethereum_types.quantity, rw) path
  | Evm_legacy_account_nonce :
      Ethereum_types.address
      -> (Ethereum_types.quantity, rw) path
  | Evm_legacy_account_code :
      Ethereum_types.address
      -> (Ethereum_types.hex, rw) path
  | Evm_legacy_account_code_hash :
      Ethereum_types.address
      -> (Ethereum_types.hash, ro) path
  | Evm_legacy_block_by_hash :
      Ethereum_types.block_hash
      -> ( Ethereum_types.legacy_transaction_object Ethereum_types.block,
           ro )
         path
  | Evm_legacy_current_block :
      (Ethereum_types.legacy_transaction_object Ethereum_types.block, ro) path
  | Evm_code_by_hash : Ethereum_types.hash -> (Ethereum_types.hex, rw) path
  | Evm_account_storage :
      Durable_storage_path.Accounts.fixed_address
      * Durable_storage_path.Accounts.fixed_index
      -> (Ethereum_types.hex, rw) path
  | Evm_account_info : Ethereum_types.address -> (EVM_account_info.t, rw) path
  | Tezos_account_info :
      Tezosx.Tezos_runtime.address
      -> (Tezosx.Tezos_runtime.account_info, ro) path
  | Evm_block_hash_by_number :
      Durable_storage_path.Block.number
      -> (Ethereum_types.block_hash, ro) path
  | Evm_transaction_receipt_by_hash :
      Ethereum_types.hash * Ethereum_types.block_hash
      -> (Transaction_receipt.t, ro) path
  | Evm_transaction_object_by_hash :
      Ethereum_types.hash * Ethereum_types.block_hash option
      -> (Ethereum_types.legacy_transaction_object, ro) path
  | Evm_current_block_receipts :
      Ethereum_types.block_hash
      -> (Transaction_receipt.t list, ro) path
  | Evm_current_block_transactions_objects :
      Ethereum_types.block_hash
      -> (Ethereum_types.legacy_transaction_object list, ro) path
  | Tezos_contract_storage :
      Tezos_types.Contract.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezos_contract_code :
      Tezos_types.Contract.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezos_big_map_value :
      Tezlink_imports.Imported_context.Big_map.Id.t
      * Tezlink_imports.Imported_protocol.Script_expr_hash.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezos_big_map_key_type :
      Tezlink_imports.Imported_context.Big_map.Id.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezos_big_map_value_type :
      Tezlink_imports.Imported_context.Big_map.Id.t
      -> (Tezlink_imports.Imported_context.Script.expr, ro) path
  | Tezlink_balance : Tezos_types.Contract.t -> (Tezos_types.Tez.t, ro) path
  | Tezlink_manager :
      Tezos_types.Contract.implicit
      -> (Tezos_types.Manager.t option, ro) path
  | Tezlink_counter : Tezos_types.Contract.implicit -> (Z.t option, ro) path
  | Blueprint_current_generation : (Ethereum_types.quantity, ro) path
  | Kernel_boot_wasm : (bytes, rw) path
  | Kernel_verbosity : (string, rw) path

(** {2 Typed operations} *)

(** [storage_version state] reads the kernel storage-version integer from
    [state], or [0] if no version marker is present. *)
val storage_version : Pvm.State.t -> int tzresult Lwt.t

(** [read p state] decodes the value at path [p] from [state] and fails if
    no value is present. *)
val read : ('a, [> `Read]) path -> Pvm.State.t -> 'a tzresult Lwt.t

(** [read_opt p state] is [Some v] when a value is present at [p], and [None]
    otherwise. *)
val read_opt : ('a, [> `Read]) path -> Pvm.State.t -> 'a option tzresult Lwt.t

(** [read_or_default ~default p state] is the value at [p], or [default] if
    none is present. Equivalent to [Option.value ~default] composed with
    [read_opt]. *)
val read_or_default :
  default:'a -> ('a, [> `Read]) path -> Pvm.State.t -> 'a tzresult Lwt.t

(** [write p value state] encodes [value] and stores it at [p]. *)
val write : ('a, rw) path -> 'a -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

(** [delete p state] removes the value stored at [p]. *)
val delete : ('a, [> `Delete]) path -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

(** [exists p state] is [true] iff a value is stored at the exact leaf
    path [p]. For directory checks, use {!exists_dir} on a {!dir}. *)
val exists : ('a, 'cap) path -> Pvm.State.t -> bool tzresult Lwt.t

(** [write_all pairs state] writes each [(path, value)] pair in order. The
    storage version is read at most once across the whole batch — lazily on
    the first {!Versioned} path. Equivalent to folding {!write} over the
    list, but without redundant version reads. *)
val write_all :
  (('a, rw) path * 'a) list -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

(** [list_runtimes state] enumerates the TezosX runtimes whose feature
    flag is set in [state]. *)
val list_runtimes : Pvm.State.t -> Tezosx.runtime list tzresult Lwt.t

(** Directory paths in the durable storage. Unlike {!path}, a [dir] does
    not carry a decoded value type — directory operations work on the
    subtree as a whole (existence, listing of subkeys, recursive delete).
    [Raw_dir] is the escape hatch for paths not yet modeled; new code
    should add a typed constructor instead. *)
type dir =
  | Raw_dir of string
  | Delayed_inbox
  | Delayed_transactions
  | Evm_events
  | Transaction_receipts
  | Transaction_objects
  | Michelson_runtime_contracts_index
  | Michelson_runtime_ledger
  | Evm_account_storage_dir of Durable_storage_path.Accounts.fixed_address

(** [delete_dir d state] recursively removes the subtree rooted at [d]. *)
val delete_dir : dir -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

(** [exists_dir d state] is [true] iff the subtree rooted at [d] is
    non-empty (i.e. has at least one descendant). *)
val exists_dir : dir -> Pvm.State.t -> bool tzresult Lwt.t

(** [subkeys d state] lists the immediate subkeys directly under [d]. *)
val subkeys : dir -> Pvm.State.t -> string trace tzresult Lwt.t

(** Raised by the legacy block readers when no block is found at the
    requested path. *)
exception Block_not_found of string
