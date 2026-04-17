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

(** {2 Typed path GADT} *)

type _ path =
  | Raw_path : string -> bytes path
  | Chain_id : L2_types.chain_id path
  | Michelson_runtime_chain_id : L2_types.chain_id path
  | Kernel_version : string path
  | Kernel_root_hash : Ethereum_types.hex path
  | Multichain_flag : unit path
  | Sequencer_key : Signature.Public_key.t path
  | Chain_config_family : L2_types.chain_id -> L2_types.ex_chain_family path
  | Tezosx_feature_flag : Tezosx.runtime -> unit path
  | Michelson_runtime_sunrise_level : Ethereum_types.quantity path
  | Current_block_number :
      _ L2_types.chain_family
      -> Ethereum_types.quantity path

(** {2 Typed operations} *)

val storage_version : Pvm.State.t -> int tzresult Lwt.t

val read : 'a path -> Pvm.State.t -> 'a tzresult Lwt.t

val read_opt : 'a path -> Pvm.State.t -> 'a option tzresult Lwt.t

val write : 'a path -> 'a -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

val delete : 'a path -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

val delete_dir : 'a path -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

val exists : 'a path -> Pvm.State.t -> bool tzresult Lwt.t

val subkeys : 'a path -> Pvm.State.t -> string trace tzresult Lwt.t

val list_runtimes : Pvm.State.t -> Tezosx.runtime list tzresult Lwt.t

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
