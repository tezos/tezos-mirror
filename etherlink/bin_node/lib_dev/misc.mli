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

(** Same as [with_timing], but (1) [event] receives the result of [k] in
    addition to the time necessary to compute it, and [k] is in the error
    monad, not just the Lwt monad. *)
val with_timing_f_e :
  ('a -> Ptime.span -> unit Lwt.t) ->
  (unit -> ('a, 'e) result Lwt.t) ->
  ('a, 'e) result Lwt.t

(** [unwrap_error_monad f] execute f and fails with a Failure when the
    error monad returns an error. *)
val unwrap_error_monad : (unit -> 'a tzresult Lwt.t) -> 'a Lwt.t

(** [normalize_addr addr] normalized an L2 address [addr],
    i.e. lowercase it and remove prefix "0x". *)
val normalize_addr : string -> string

(** [interpolate str vars] computes a new string when the variables specified
    in [vars] are replaced by their value.

    We use a format similar to [printf], that is [%<c>] where [<c>] is a
    character. [vars] is therefore a list of pair containing a character (the
    variable) and a string (its value). *)
val interpolate : string -> (char * string) list -> string

val download_file :
  keep_alive:bool -> working_dir:string -> string -> string tzresult Lwt.t
