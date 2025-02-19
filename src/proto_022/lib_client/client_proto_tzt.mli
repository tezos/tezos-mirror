(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type unit_test_with_source = {
  source : string;
  parsed : string Michelson_v1_parser.parser_result;
}

val run_unit_test :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  test:unit_test_with_source ->
  unit ->
  unit tzresult Lwt.t
