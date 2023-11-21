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
  let all_contracts =
    let other_contracts =
      Option.value ~default:[] ut.optional.other_contracts
    in
    match (ut.optional.self, ut.optional.parameter) with
    | Some self, Some param ->
        RPC.Scripts.S.{address = self; ty = param} :: other_contracts
    | None, _ | Some _, None -> other_contracts
  in
  let* chain_id =
    match ut.optional.chain_id with
    | Some chain_id -> return chain_id
    | None -> Chain_services.chain_id cctxt ~chain ()
  in
  let amount = Option.value ~default:Tez.zero ut.optional.amount in
  let* expected_output =
    Plugin.RPC.Scripts.normalize_stack
      cctxt
      (chain, block)
      ~stack:ut.output
      ~unparsing_mode:Readable
      ~legacy:true
      ~other_contracts:(Some all_contracts)
      ~extra_big_maps:ut.optional.extra_big_maps
  in
  let* output, _gas =
    Plugin.RPC.Scripts.run_instr
      ~legacy:true
      ~gas:None
      ~input:ut.input
      ~code:ut.code
      ~now:ut.optional.now
      ~level:ut.optional.level
      ~sender:ut.optional.sender
      ~source:ut.optional.source
      ~chain_id
      ~self:ut.optional.self
      ~parameter:ut.optional.parameter
      ~amount
      ~balance:ut.optional.balance
      ~other_contracts:(Some all_contracts)
      ~extra_big_maps:ut.optional.extra_big_maps
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
