(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

type parameters = {
  rollup_node_endpoint : Uri.t;
      (** Rollup node endpoint used to monitor the delayed inbox. *)
  delayed_inbox_interval : int;
      (** Number of levels every which the worker will fetch the
          delayed inbox. *)
}

(** [start parameters] starts the delayed inbox worker. *)
val start : parameters -> unit tzresult Lwt.t

(** [shutdown ()] stops the delayed inbox worker. *)
val shutdown : unit -> unit Lwt.t
