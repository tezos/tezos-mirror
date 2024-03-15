(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** [now ()] returns the current time. *)
val now : unit -> Time.Protocol.t

(** [with_timing event k] computes how much time [k ()] takes to be computed
    and advertises it with [event]. *)
val with_timing : (Ptime.span -> unit Lwt.t) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
