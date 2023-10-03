(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val create :
  ?name:string ->
  ?color:Log.Color.t ->
  ?data_dir:string ->
  ?event_pipe:string ->
  ?uri:Uri.t ->
  ?runner:Runner.t ->
  Node.t ->
  Client.t ->
  t

val init_config : t -> Account.key -> string Lwt.t

val run : t -> unit Lwt.t

module RPC : sig
  type status =
    | Pending
    | Injected of {injected_oph : string; injected_op_index : int}
    | Included of {
        included_oph : string;
        included_op_index : int;
        block : string;
        level : int;
      }

  val add_pending_transaction :
    ?parameters:string * string -> int64 -> string -> string RPC_core.t

  val operation_status : string -> status option RPC_core.t

  val inject : unit -> unit RPC_core.t

  include RPC_core.CALLERS with type uri_provider := t
end
