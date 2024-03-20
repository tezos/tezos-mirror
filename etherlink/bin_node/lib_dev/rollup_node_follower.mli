(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [start ~rollup_node_endpoint] starts the rollup node follower. *)
val start : rollup_node_endpoint:Uri.t -> unit
