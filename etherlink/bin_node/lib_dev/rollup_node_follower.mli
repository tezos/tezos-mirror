(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  rollup_node_endpoint : Uri.t;
      (** Rollup node endpoint used to monitor the stream of rollup
          node block. *)
}

(** [start parameters] starts the rollup node follower. *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the rollup node follower. *)
val shutdown : unit -> unit Lwt.t
