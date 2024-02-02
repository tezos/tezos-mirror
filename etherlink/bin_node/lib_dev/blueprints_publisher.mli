(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val start :
  rollup_node_endpoint:Uri.t ->
  max_blueprints_lag:int ->
  max_blueprints_catchup:int ->
  catchup_cooldown:int ->
  Blueprint_store.t ->
  unit tzresult Lwt.t

val shutdown : unit -> unit Lwt.t

(** [publish level payload] sends a request to the publisher worker to
    forward the chunked blueprint [payload] for level [level] to the
    rollup node. *)
val publish : Z.t -> [`External of string] list -> unit tzresult Lwt.t

(** [new_l2_head rollup_head] tells the worker that a new L2 head has been
    published and that the rollup head is now [rollup_head]. *)
val new_l2_head : Z.t -> unit tzresult Lwt.t
