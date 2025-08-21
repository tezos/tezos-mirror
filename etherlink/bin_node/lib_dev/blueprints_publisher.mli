(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type blueprints_range =
  from:Ethereum_types.quantity ->
  to_:Ethereum_types.quantity ->
  (Ethereum_types.quantity * Blueprint_types.payload) list tzresult Lwt.t

val start :
  blueprints_range:blueprints_range ->
  rollup_node_endpoint:Uri.t ->
  config:Configuration.blueprints_publisher_config ->
  latest_level_seen:Z.t ->
  keep_alive:bool ->
  drop_duplicate:bool ->
  order_enabled:bool ->
  tx_container:_ Services_backend_sig.tx_container ->
  unit ->
  unit tzresult Lwt.t

val shutdown : unit -> unit tzresult Lwt.t

(** [publish level payload] sends a request to the publisher worker to
    forward the chunked blueprint [payload] for level [level] to the
    rollup node. *)
val publish :
  Z.t -> Blueprints_publisher_types.Request.payload -> unit tzresult Lwt.t

(** [new_rollup_block rollup_level] tells the worker that a new rollup
    node block. *)
val new_rollup_block : int32 -> unit tzresult Lwt.t
