(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)
let env_var = "TEZOS_SIGN_DELAY_I_KNOW_WHAT_I_AM_DOING"

(** Enforce that if the environment variable [signing_delay_env_var] is set,
    then signing delay is allowed in the current configuration.

    If not, print an error message and exit the program with code 1. *)
let enforce_signing_delay_gating ~allow =
  match Sys.getenv_opt env_var with
  | Some _ when not allow ->
      Stdlib.failwith
        (Format.sprintf
           "@{<error>@{<title>Fatal error@}@} %s is set, but signing delay is \
            not allowed in the current configuration.@."
           env_var)
  | _ -> ()

let pp_float fmt f = Format.fprintf fmt "%.4f" f

let section = ["Baking_common"]

module Events = struct
  include Internal_event.Simple

  let section = section @ ["signing"; "delay"]

  let signing_delay_is_enabled =
    declare_3
      ~level:Warning
      ~section
      ~name:"signing_delay_is_enabled"
      ~msg:
        "Signing delay is enabled with a range of {min} {max} seconds, \
         selected delay will be {actual_delay}."
      ~pp1:pp_float
      ("min", Data_encoding.float)
      ~pp2:pp_float
      ("max", Data_encoding.float)
      ~pp3:pp_float
      ("actual_delay", Data_encoding.float)

  let signing_delay_invalid_argument =
    declare_1
      ~level:Error
      ~section
      ~name:"signing_delay_invalid_argument"
      ~msg:
        "The signing delay argument is invalid: {error}. It accepts two forms: \
         a single positive float representing the maximum delay in seconds, or \
         a range of the form <min>,<max> where min and max are positive floats \
         with 0 <= min <= max. Signing delay is disabled."
      ~pp1:Format.pp_print_string
      ("error", Data_encoding.string)
end

(** [artificial_delay_opt] is [None] if no artificial delay is configured, or
    [Some d] where [d] is a random delay in seconds to wait before signing.

    The environment variable [TEZOS_SIGN_DELAY_I_KNOW_WHAT_I_AM_DOING] can be set
    to configure an artificial delay. Its value must be either:

    - a single non-negative float [d], in which case the delay will be a random
      value uniformly distributed in [0, d];

    - two non-negative floats [d1, d2] separated by a comma, with [d1 <= d2],
      in which case the delay will be a random value uniformly distributed in
      [d1, d2].

    If the environment variable is set but has an invalid value, then no delay
    is configured and an error message is printed. *)
let artificial_delay_opt =
  let v = Sys.getenv_opt env_var in
  (* PRNG Initialisation is made at client initialisation, here we compute the
     delay at toplevel so we need to init PRNG with the correct seed if
     needed *)
  (match
     Sys.getenv_opt
       Tezos_client_base_unix.Client_config.client_fixed_random_seed_env_var
   with
  | None -> Random.self_init ()
  | Some seed_str -> (
      match int_of_string_opt seed_str with
      | None ->
          Tezos_client_base_unix.Client_main_run.Events.(
            emit_at_top_level rand_seed_is_not_an_int)
            seed_str ;
          Random.self_init ()
      | Some seed ->
          Tezos_client_base_unix.Client_main_run.Events.(
            emit_at_top_level rand_seed_is_set)
            seed ;
          Random.init seed)) ;
  Option.bind v (fun s ->
      let parsed =
        match String.split_on_char ',' s with
        | [d] -> (
            match float_of_string_opt d with
            | Some d when 0. <= d -> Ok (0., d, Random.float d)
            | _ -> Error ())
        | [d1; d2] -> (
            match (float_of_string_opt d1, float_of_string_opt d2) with
            | Some d1, Some d2 when 0. <= d1 && d1 <= d2 ->
                Ok (d1, d2, Random.float (d2 -. d1) +. d1)
            | _ -> Error ())
        | _ -> Error ()
      in
      match parsed with
      | Ok (min, max, d) ->
          Events.(emit_at_top_level signing_delay_is_enabled (min, max, d)) ;
          Some d
      | Error () ->
          Events.(emit_at_top_level signing_delay_invalid_argument s) ;
          None)

(** [sign_with_minimum_duration (module Profiler) sign_f] runs [sign_f ()] and ensures the
    call takes at least the configured artificial delay.
    A profiler module is needed to ensure correct profiling of the signature.

    If [artificial_delay_opt = Some d], it runs [sign_f ()] and waits for a
    delay of [d] seconds concurrently. The function returns the result of the
    signing only after both the signing operation and the delay have
    elapsed. This makes the total duration of the call at least [d] seconds.

    If no delay is configured, it just runs [sign_f ()]. *)
let sign_with_minimum_duration (module Profiler : Profiler.GLOBAL_PROFILER) =
  let open Lwt_syntax in
  match artificial_delay_opt with
  | None -> fun sign_f -> sign_f ()
  | Some d ->
      fun sign_f ->
        ((let sign_t = sign_f () in
          let delay_t = Lwt_unix.sleep d in
          let sign_ignored_result_t =
            let* _ = sign_t in
            return_unit
          in
          let* () = Lwt.join [delay_t; sign_ignored_result_t] in
          sign_t)
        [@profiler.record_s
          {verbosity = Debug}
            (Format.sprintf "sign with minimum duration %.4fs" d)])
