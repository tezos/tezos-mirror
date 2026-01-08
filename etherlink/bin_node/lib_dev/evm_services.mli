(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type 'a monitor

val close_monitor : 'a monitor -> unit

val get_from_monitor : 'a monitor -> 'a option Lwt.t

val get_smart_rollup_address :
  keep_alive:bool ->
  timeout:float ->
  Uri.t ->
  Tezos_crypto.Hashed.Smart_rollup_address.t tzresult Lwt.t

val get_time_between_blocks :
  ?fallback:Configuration.time_between_blocks ->
  timeout:float ->
  evm_node_endpoint:Uri.t ->
  unit ->
  Configuration.time_between_blocks tzresult Lwt.t

val get_blueprint :
  keep_alive:bool ->
  timeout:float ->
  evm_node_endpoint:Uri.t ->
  Ethereum_types.quantity ->
  Blueprint_types.Legacy.with_events tzresult Lwt.t

val get_blueprints :
  keep_alive:bool ->
  timeout:float ->
  evm_node_endpoint:Uri.t ->
  count:int64 ->
  Ethereum_types.quantity ->
  Blueprint_types.Legacy.with_events list tzresult Lwt.t

val get_blueprint_with_events :
  keep_alive:bool ->
  timeout:float ->
  evm_node_endpoint:Uri.t ->
  Ethereum_types.quantity ->
  Blueprint_types.with_events tzresult Lwt.t

val get_blueprints_with_events :
  keep_alive:bool ->
  timeout:float ->
  evm_node_endpoint:Uri.t ->
  count:int64 ->
  Ethereum_types.quantity ->
  Blueprint_types.with_events list tzresult Lwt.t

val register :
  (unit -> Ethereum_types.quantity Lwt.t) ->
  (Ethereum_types.quantity ->
  Blueprint_types.Legacy.with_events option tzresult Lwt.t) ->
  (Ethereum_types.quantity -> Blueprint_types.with_events option tzresult Lwt.t) ->
  Tezos_crypto.Hashed.Smart_rollup_address.t ->
  Configuration.time_between_blocks ->
  Evm_directory.t ->
  Evm_directory.t

val monitor_blueprints :
  evm_node_endpoint:Uri.t ->
  timeout:float ->
  Ethereum_types.quantity ->
  Blueprint_types.Legacy.with_events Lwt_stream.t tzresult Lwt.t

val monitor_messages :
  evm_node_endpoint:Uri.t ->
  timeout:float ->
  Ethereum_types.quantity ->
  Broadcast.message monitor tzresult Lwt.t
