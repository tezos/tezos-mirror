(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Utility module for experimental baker variables in cloud tests *)

open Tezt_wrapper.Base

(* Helper to add signing delay range to environment *)
let signing_delay_env min max env =
  String_map.add
    Client.signing_delay_env_var
    (Format.sprintf "%f,%f" min max)
    env

(* Helper to add fixed random seed to environment *)
let fixed_seed_env env =
  String_map.add
    Client.fixed_seed_env_var
    (string_of_int @@ Random.int 1000000)
    env
