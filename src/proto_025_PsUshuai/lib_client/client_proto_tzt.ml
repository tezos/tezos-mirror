(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
open Tezos_micheline

type unit_test_with_source = {
  source : string;
  parsed : string Michelson_v1_parser.parser_result;
}

let convert_error trace =
  let open Result_syntax in
  let open Micheline in
  function
  | Script_interpreter_errors.Reject (_loc, value, _trace) ->
      let value = Michelson_v1_primitives.strings_of_prims value in
      return (Prim (0, "Failed", [root value], []))
  | Tez_repr.Addition_overflow _ | Tez_repr.Multiplication_overflow _
  | Script_interpreter_errors.Overflow _ ->
      return (Prim (0, "Overflow", [], []))
  | Tez_repr.Subtraction_underflow (a, b) ->
      return
        (Prim
           ( 0,
             "MutezUnderflow",
             [
               Int (0, Z.of_int64 @@ Tez_repr.to_mutez a);
               Int (0, Z.of_int64 @@ Tez_repr.to_mutez b);
             ],
             [] ))
  | Tez_repr.Negative_multiplicator _ -> return (Prim (0, "NegMul", [], []))
  | Tez_repr.Invalid_divisor _ -> return (Prim (0, "InvalidDivisor", [], []))
  | Raw_context.Operation_quota_exceeded | Raw_context.Block_quota_exceeded ->
      return (Prim (0, "Gas_exhaustion", [], []))
  | _ ->
      return
        (Prim
           ( 0,
             "StaticError",
             [String (0, Format.asprintf "%a" Error_monad.pp_print_trace trace)],
             [] ))

let convert_trace = function
  | Environment.Ecoproto_error err :: _ as trace -> convert_error trace err
  | _ -> assert false

let match_output ~got ~expected =
  let open Result_syntax in
  let open Micheline in
  let rec match_pattern pattern expression =
    match (pattern, expression) with
    (* Wildcard *)
    | Prim (_, "_", [], []), _ -> true
    (* Int *)
    | Int (_p_loc, p), Int (_e_loc, e) -> Z.equal p e
    | Int _, _ | _, Int _ -> false
    (* String *)
    | String (_p_loc, p), String (_e_loc, e) -> Compare.String.(p = e)
    | String _, _ | _, String _ -> false
    (* Bytes *)
    | Bytes (_p_loc, p), Bytes (_e_loc, e) -> Compare.Bytes.(p = e)
    | Bytes _, _ | _, Bytes _ -> false
    (* Seq *)
    | Seq (_p_loc, p), Seq (_e_loc, e) -> (
        match List.for_all2 ~when_different_lengths:() match_pattern p e with
        | Ok b -> b
        | Error () -> false)
    | Seq _, _ | _, Seq _ -> false
    (* Prim *)
    | ( Prim (_p_loc, p_prim, p_args, p_annots),
        Prim (_e_loc, e_prim, e_args, e_annots) ) -> (
        Compare.String.(p_prim = "_" || p_prim = e_prim)
        && (match
              List.for_all2
                ~when_different_lengths:()
                match_pattern
                p_args
                e_args
            with
           | Ok b -> b
           | Error () -> false)
        &&
        match
          List.for_all2
            ~when_different_lengths:()
            Compare.String.( = )
            p_annots
            e_annots
        with
        | Ok b -> b
        | Error () -> false)
  in
  if match_pattern expected got then return_unit
  else
    let pp fmt e =
      Micheline_printer.print_expr_unwrapped
        fmt
        (Micheline_printer.printable Fun.id (Micheline.strip_locations e))
    in
    (* TODO: proper error instead of failwith *)
    error_with "Got output: %a@.Expected: %a@." pp got pp expected

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
  let*! res =
    match ut.output with
    | Micheline.Seq _ as stack ->
        let*? stack = Michelson_v1_stack.parse_stack ~node:stack test.parsed in
        let* stack =
          Plugin.RPC.Scripts.normalize_stack
            cctxt
            (chain, block)
            ~stack
            ~unparsing_mode:Readable
            ~legacy:true
            ~other_contracts:(Some all_contracts)
            ~extra_big_maps:ut.optional.extra_big_maps
        in
        return (Michelson_v1_printer.unparse_stack 0 stack)
    | expected_output -> return expected_output
  in
  (* Wildcard patterns in output stack is incompatible with output stack normalization.
     When the output stack contains a wildcard pattern, the normalization is expected
     to fail. To support wildcard patterns, we silently skip output stack normalization
     when it fails. *)
  let expected_output = match res with Ok x -> x | Error _ -> ut.output in
  let*! res =
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
  let*? output =
    match res with
    | Ok (output, _gas) ->
        Result_syntax.return (Michelson_v1_printer.unparse_stack 0 output)
    | Error err -> convert_trace err
  in
  let*? () = match_output ~expected:expected_output ~got:output in
  return_unit
