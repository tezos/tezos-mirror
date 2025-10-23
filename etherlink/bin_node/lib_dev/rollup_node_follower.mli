(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [start ~keep_alive ?on_new_head ~rollup_node_endpoint ()] starts
    the rollup node follower. When [on_new_head] is given then it's
    call at each received block. *)
val start :
  keep_alive:bool ->
  ?on_new_head:(unit -> unit tzresult Lwt.t) ->
  rollup_node_endpoint:Uri.t ->
  rollup_node_endpoint_timeout:float ->
  unit ->
  unit
