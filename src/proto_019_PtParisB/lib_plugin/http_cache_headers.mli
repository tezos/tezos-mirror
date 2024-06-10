(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** [get_round_end_time get_context header] gets the time at which the 
    current round ends which is the time at which the next round starts. 
    Useful to get an estimate of when the next block should arrive. 
    [get_context] is called the first time this function is called to 
    get the current constants. *)
val get_round_end_time :
  get_context:(unit -> Tezos_protocol_environment.Context.t Lwt.t) ->
  Block_header.shell_header ->
  Time.System.t option Lwt.t
