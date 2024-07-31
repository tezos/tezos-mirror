(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [start ()] starts the signal
    publisher. It should only be called once and only if DAL is
    enabled. *)
val start : unit -> unit tzresult Lwt.t

(** [shutdown ()] shuts down the signal publisher. *)
val shutdown : unit -> unit tzresult Lwt.t
