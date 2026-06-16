(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Protocol

let constants_key = [Constants_repr.version; "constants"]

let durations : Round_repr.Durations.t option ref = ref None

let get_constants ctxt =
  let open Lwt_syntax in
  let* bytes_opt = Tezos_protocol_environment.Context.find ctxt constants_key in
  match bytes_opt with
  | Some bytes ->
      return
      @@ Data_encoding.Binary.of_bytes_opt
           Constants_parametric_repr.encoding
           bytes
  | None -> return_none

let get_durations ctx =
  let open Lwt_option_syntax in
  match !durations with
  | Some durations -> return durations
  | None ->
      let* {minimal_block_delay; delay_increment_per_round; _} =
        get_constants ctx
      in
      Lwt.return
      @@ Result.to_option
           (Round_repr.Durations.create
              ~first_round_duration:minimal_block_delay
              ~delay_increment_per_round)

let get_round_end_time ~get_context
    (curr_header : Tezos_base.Block_header.shell_header) =
  let open Lwt_option_syntax in
  let* durations =
    match !durations with
    | Some durations -> return durations
    | None ->
        let* ctx = Lwt.map Option.some (get_context ()) in
        get_durations ctx
  in
  let* round_duration =
    let+ round =
      Lwt.return
      @@ Result.to_option (Fitness_repr.round_from_raw curr_header.fitness)
    in
    Round_repr.Durations.round_duration durations round
  in
  let round_end_timestamp =
    Time.Protocol.add
      curr_header.timestamp
      (Period_repr.to_seconds round_duration)
  in
  return (Time.System.of_protocol_exn round_end_timestamp)
