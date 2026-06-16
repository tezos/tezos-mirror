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

(** Shows on stdout all events sent by the injector *)
val log_events : ?max_length:int -> t -> unit

(** See [Daemon.Make.wait_for]. *)
val wait_for :
  ?timeout:float ->
  ?where:string ->
  t ->
  string ->
  (JSON.t -> 'a option) ->
  'a Lwt.t

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
