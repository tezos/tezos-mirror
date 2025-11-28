(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A description of the metrics exported by the node. *)
val listing : unit -> string Lwt.t

type metrics_body = {body : string; content_type : string}

val get_metrics : unit -> metrics_body Lwt.t

module Metrics_server : sig
  val callback :
    Cohttp_lwt_unix.Server.conn ->
    Cohttp.Request.t ->
    Cohttp_lwt.Body.t ->
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t
end

module Tx_pool : sig
  type size_info = {number_of_addresses : int; number_of_transactions : int}
end

val init :
  mode:string ->
  ?tx_pool_size_info:(unit -> Tx_pool.size_info tzresult Lwt.t) ->
  smart_rollup_address:Tezos_crypto.Hashed.Smart_rollup_address.t ->
  unit ->
  unit

val set_level : level:Z.t -> unit

val set_gas_price : Z.t -> unit

val set_confirmed_level : level:Z.t -> unit

val set_l1_level : level:int32 -> unit

val start_bootstrapping : unit -> unit

val stop_bootstrapping : unit -> unit

val start_pruning : unit -> unit

val stop_pruning : unit -> unit

(** [inc_time_waiting dt] registers that a caller had to wait [dt] seconds in
    the queue before its WASM execution could start. *)
val inc_time_waiting : Ptime.span -> unit

(** [set_simulation_queue_size s] registers that the queue of WASM execution. *)
val set_simulation_queue_size : int -> unit

val is_bootstrapping : unit -> bool

val set_block : time_processed:Ptime.span -> transactions:int -> unit

val record_signals_sent : (int * int32) list -> unit

(** [inc_confirm_gas_needed ()] registers that it was necessary to simulate gas
    one more time, because the previous simulation did not return enough gas. *)
val inc_confirm_gas_needed : unit -> unit

val record_blueprint_chunks_sent_on_dal :
  Sequencer_blueprint.chunked_blueprint -> unit

val record_blueprint_chunks_sent_on_inbox : Blueprint_types.chunk list -> unit

val inc_rpc_method : name:string -> unit

module Rpc : sig
  (** [update_metrics uri method_name callback] updates the metrics related to
      the given [uri] and more specifically the given [method_name].

      This function is expected to be passed as an argument of
      {!Tezos_rpc_http_server.RPC_middleware.rpc_metrics_transform_callback}. *)
  val update_metrics :
    string ->
    string ->
    (unit -> Cohttp_lwt_unix.Server.response_action Lwt.t) ->
    Cohttp_lwt_unix.Server.response_action Lwt.t
end

module type PERFORMANCE = sig
  val set_stats : data_dir:string -> unit
end

val performance_metrics : (module PERFORMANCE) Lazy.t
