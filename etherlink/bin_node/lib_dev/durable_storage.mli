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

(** {2 Typed path GADT}

    A path carries two phantom parameters: the decoded value type ['a]
    and a capability row ['cap] made of the polymorphic-variant tags
    [`Read] and [`Write]. The alias {!rw} (read+write) names the row
    every current path uses. [Raw_path] is the escape hatch that
    decodes to raw bytes; new code should add typed constructors with
    the precise capability row instead. *)

(** Phantom capability marker for [path]: [rw] is read+write. *)
type rw = [`Read | `Write]

type ('a, 'cap) path =
  | Raw_path : string -> (bytes, rw) path
  | Chain_id : (L2_types.chain_id, rw) path
  | Michelson_runtime_chain_id : (L2_types.chain_id, rw) path
  | Kernel_version : (string, rw) path
  | Kernel_root_hash : (Ethereum_types.hex, rw) path
  | Multichain_flag : (unit, rw) path
  | Sequencer_key : (Signature.Public_key.t, rw) path
  | Chain_config_family :
      L2_types.chain_id
      -> (L2_types.ex_chain_family, rw) path
  | Tezosx_feature_flag : Tezosx.runtime -> (unit, rw) path
  | Michelson_runtime_sunrise_level : (Ethereum_types.quantity, rw) path
  | Current_block_number :
      _ L2_types.chain_family
      -> (Ethereum_types.quantity, rw) path
  | Current_block_hash :
      _ L2_types.chain_family
      -> (Ethereum_types.block_hash, rw) path
  | Evm_node_flag : (unit, rw) path
  | Blueprint_chunk : {
      blueprint_number : Z.t;
      chunk_index : int;
    }
      -> (bytes, rw) path
  | Blueprint_nb_chunks : Z.t -> (int, rw) path
  | Blueprint_generation : Z.t -> (Ethereum_types.quantity, rw) path

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

(** [write p value state] encodes [value] and stores it at [p]. *)
val write : ('a, rw) path -> 'a -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

(** [delete p state] removes the value stored at [p]. *)
val delete : ('a, rw) path -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

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

(** [delete_dir d state] recursively removes the subtree rooted at [d]. *)
val delete_dir : dir -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

(** [exists_dir d state] is [true] iff the subtree rooted at [d] is
    non-empty (i.e. has at least one descendant). *)
val exists_dir : dir -> Pvm.State.t -> bool tzresult Lwt.t

(** [subkeys d state] lists the immediate subkeys directly under [d]. *)
val subkeys : dir -> Pvm.State.t -> string trace tzresult Lwt.t

(** {2 Deprecated untyped API}

    The functions below operate on raw durable storage paths (strings) with
    manual decoders. They will be removed once all callers have migrated to the
    typed GADT API above ({!read}, {!read_opt}, {!write}, ...).

    @deprecated Use the typed path API instead. *)

(** @deprecated Use the typed path API instead. *)
exception Invalid_block_structure of string

(** @deprecated Use {!read_opt} instead. *)
val inspect_durable_and_decode_opt :
  Pvm.Context.tree ->
  string ->
  (bytes -> 'a) ->
  ('a option, tztrace) result Lwt.t

(** @deprecated Use {!read_opt} with [Option.value] instead. *)
val inspect_durable_and_decode_default :
  default:'a ->
  Pvm.Context.tree ->
  string ->
  (bytes -> 'a) ->
  ('a, tztrace) result Lwt.t

(** @deprecated Use {!read} instead. *)
val inspect_durable_and_decode :
  Pvm.Context.tree -> string -> (bytes -> 'a) -> ('a, tztrace) result Lwt.t

(** @deprecated Use the typed path API instead. *)
val l2_minimum_base_fee_per_gas :
  Pvm.Context.tree -> L2_types.chain_id -> Z.t tzresult Lwt.t

(** @deprecated Use the typed path API instead. *)
val l2_da_fee_per_byte :
  Pvm.Context.tree -> L2_types.chain_id -> Z.t tzresult Lwt.t

(** @deprecated Use the typed path API instead. *)
val l2_maximum_gas_per_transaction :
  Pvm.Context.tree -> L2_types.chain_id -> Z.t tzresult Lwt.t

(** @deprecated Use the typed path API instead. *)
val world_state : Pvm.Context.tree -> L2_types.chain_id -> string tzresult Lwt.t
