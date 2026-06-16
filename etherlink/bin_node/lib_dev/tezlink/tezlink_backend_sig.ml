(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Paths through [or] types that lead to unreachable entrypoints. *)
type unreachable_paths =
  Tezlink_imports.Imported_protocol.Michelson_v1_primitives.prim list list

(** Named entrypoints with their Micheline type expressions. *)
type named_entrypoints =
  (string * Tezlink_imports.Imported_context.Script.expr) list

(** Unreachable entrypoint paths paired with the named entrypoint type
    expressions.  Matches the L1 [list_entrypoints] RPC output shape. *)
type entrypoints_info = unreachable_paths * named_entrypoints

(* In simulation mode, skip signature check and allow null fees. In
   preapplication mode, check signature and fees. *)
type simulator_mode = Simulation | Preapplication

let simulator_mode_encoding =
  Data_encoding.string_enum
    [("simulation", Simulation); ("preapplication", Preapplication)]

module type S = sig
  type block_param =
    [ `Head of int32
    | `Level of int32
    | `Hash of Ethereum_types.block_hash * int32 ]

  val current_level :
    [`Main] -> block_param -> offset:int32 -> Tezos_types.level tzresult Lwt.t

  val constants : [`Main] -> block_param -> Tezlink_constants.t tzresult Lwt.t

  val balance :
    [`Main] ->
    block_param ->
    Tezos_types.Contract.t ->
    Tezos_types.Tez.t tzresult Lwt.t

  val list_contracts :
    [`Main] -> block_param -> Tezos_types.Contract.t list tzresult Lwt.t

  val get_storage :
    [`Main] ->
    block_param ->
    Tezos_types.Contract.t ->
    Tezlink_imports.Imported_context.Script.expr option tzresult Lwt.t

  val get_code :
    [`Main] ->
    block_param ->
    Tezos_types.Contract.t ->
    Tezlink_imports.Imported_context.Script.expr option tzresult Lwt.t

  val get_script :
    [`Main] ->
    block_param ->
    Tezos_types.Contract.t ->
    Tezlink_imports.Imported_context.Script.t option tzresult Lwt.t

  val manager_key :
    [`Main] ->
    block_param ->
    Tezos_types.Contract.t ->
    Signature.V2.Public_key.t option tzresult Lwt.t

  val counter :
    [`Main] ->
    block_param ->
    Tezos_types.Contract.t ->
    Z.t option tzresult Lwt.t

  val big_map_get :
    [`Main] ->
    block_param ->
    Tezlink_imports.Imported_context.Big_map.Id.t ->
    Tezlink_imports.Imported_protocol.Script_expr_hash.t ->
    Tezlink_imports.Imported_context.Script.expr option tzresult Lwt.t

  val big_map_raw_info :
    [`Main] ->
    block_param ->
    Tezlink_imports.Imported_context.Big_map.Id.t ->
    (Tezlink_imports.Imported_context.Script.expr
    * Tezlink_imports.Imported_context.Script.expr
    * Z.t
    * Tezlink_imports.Imported_context.Script.expr list)
    option
    tzresult
    Lwt.t

  val block : [`Main] -> block_param -> L2_types.Tezos_block.t tzresult Lwt.t

  val bootstrapped :
    unit -> (Ethereum_types.block_hash * Time.Protocol.t) tzresult Lwt.t

  val block_hash :
    [`Main] -> block_param -> Ethereum_types.block_hash option tzresult Lwt.t

  val monitor_heads :
    [> `Main] -> 'a -> L2_types.Tezos_block.t Lwt_stream.t * Lwt_watcher.stopper

  val simulate_operation :
    chain_id:Chain_id.t ->
    simulator_mode:simulator_mode ->
    Tezlink_imports.Imported_protocol.operation ->
    Operation_hash.t ->
    block_param ->
    Tezlink_imports.Imported_protocol.operation_receipt tzresult Lwt.t

  val get_entrypoints :
    [`Main] ->
    block_param ->
    Tezos_types.Contract.t ->
    normalize_types:bool ->
    entrypoints_info option tzresult Lwt.t
end
