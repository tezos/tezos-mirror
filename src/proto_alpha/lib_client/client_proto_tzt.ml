(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

type unit_test_with_source = {
  source : string;
  parsed : string Michelson_v1_parser.parser_result;
}

let print_stack out l = Michelson_v1_printer.print_typed_stack out l

let run_unit_test (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block ~(test : unit_test_with_source) () =
  let open Lwt_result_syntax in
  let*? ut = Michelson_v1_stack.parse_unit_test test.parsed in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  let amount = Tez.zero in
  let* expected_output =
    Plugin.RPC.Scripts.normalize_stack
      cctxt
      (chain, block)
      ~stack:ut.output
      ~unparsing_mode:Readable
      ~legacy:true
      ~other_contracts:None
      ~extra_big_maps:None
  in
  let* output, _gas =
    Plugin.RPC.Scripts.run_instr
      ~legacy:true
      ~gas:None
      ~input:ut.input
      ~code:ut.code
      ~now:None
      ~level:None
      ~sender:None
      ~source:None
      ~chain_id
      ~self:None
      ~parameter:None
      ~amount
      ~balance:None
      ~other_contracts:None
      ~extra_big_maps:None
      ~unparsing_mode:None
      cctxt
      (chain, block)
  in
  if output = expected_output then return_unit
  else
    failwith
      "Output: %a@.Expected: %a@."
      print_stack
      output
      print_stack
      expected_output
