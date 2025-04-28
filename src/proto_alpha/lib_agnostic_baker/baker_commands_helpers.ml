(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let run_baker ~configuration ~baking_mode ~sources ~cctxt =
  let args = Baker_args_parser.parse_configuration configuration in
  let baking_mode = Baker_args_parser.parse_baking_mode baking_mode in
  let cctxt = new Protocol_client_context.wrap_full cctxt in
  Baking_commands.run_baker
    ~recommend_agnostic_baker:false
    args
    baking_mode
    sources
    cctxt

let run_vdf_daemon ~cctxt ~keep_alive =
  let cctxt = new Protocol_client_context.wrap_full cctxt in
  Client_daemon.VDF.run
    ~recommend_agnostic_baker:false
    cctxt
    ~chain:cctxt#chain
    ~keep_alive
