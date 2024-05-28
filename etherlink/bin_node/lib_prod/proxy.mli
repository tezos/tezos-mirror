(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [main config ~rollup_node_endpoint ~keep_alive] starts
    the main event-loop of the proxy using [rollup_node_endpoint]. *)
val main :
  Configuration.t ->
  keep_alive:bool ->
  rollup_node_endpoint:Uri.t ->
  unit tzresult Lwt.t
