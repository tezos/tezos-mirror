(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type config = {rollup_node_endpoint : Uri.t}

val start : config -> unit tzresult Lwt.t

val shutdown : unit -> unit Lwt.t

(** [publish level payload] sends a request to the publisher worker to
    forward the chunked blueprint [payload] for level [level] to the
    rollup node. *)
val publish : Z.t -> [`External of string] list -> unit tzresult Lwt.t
