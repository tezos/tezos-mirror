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

(** [normalize_hex str] normalized an hexa-encoded string, i.e. lowercase it
    and remove prefix "0x" if it exists. *)
val normalize_hex : string -> Hex.t tzresult

(** [interpolate str vars] computes a new string when the variables specified
    in [vars] are replaced by their value.

    We use a format similar to [printf], that is [%<c>] where [<c>] is a
    character. [vars] is therefore a list of pair containing a character (the
    variable) and a string (its value). *)
val interpolate :
  string ->
  (char * [`Available of string | `Disabled of string]) list ->
  string tzresult

(** [domain_count_cap ()] returns the number of domains the node is allowing
    itself to spawn.

    The result is always between 5 and 16. *)
val domain_count_cap : unit -> int

exception Timeout

val with_timeout : int -> (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t
