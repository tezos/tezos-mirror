(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  rollup_node_endpoint : Uri.t;
      (** Rollup node endpoint used to monitor kernel events. *)
  filter_event : Evm_events.t -> bool;
      (** Filter event the follower applies. *)
  keep_alive : bool;
}

(** [start parameters] starts the events follower. *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the events follower. *)
val shutdown : unit -> unit tzresult Lwt.t

(** [new_rollup_block rollup_level] tells the worker that a new L2
    head has been published and that the rollup head is now
    [rollup_level]. *)
val new_rollup_block : Int32.t -> unit tzresult Lwt.t
