(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val start :
  smart_rollup_address:string ->
  rollup_node_endpoint:Uri.t ->
  config:Configuration.blueprints_publisher_config ->
  latest_level_seen:Z.t ->
  keep_alive:bool ->
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
