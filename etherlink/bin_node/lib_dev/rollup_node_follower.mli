(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [start ~keep_alive ~proxy ~rollup_node_endpoint ()] starts the
    rollup node follower. In proxy mode does not try to catchup evm
    event. *)
val start :
  keep_alive:bool -> proxy:bool -> rollup_node_endpoint:Uri.t -> unit -> unit
