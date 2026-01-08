(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context
open Tezos_micheline
open Michelson_v1_printer

module Program = Client_aliases.Alias (struct
  type t = Michelson_v1_parser.parsed Micheline_parser.parsing_result

  include Compare.Make (struct
    type nonrec t = t

    let compare = Micheline_parser.compare Michelson_v1_parser.compare_parsed
  end)

  let encoding =
    Data_encoding.conv
      (fun ({Michelson_v1_parser.source; _}, _) -> source)
      (fun source -> Michelson_v1_parser.parse_toplevel source)
      Data_encoding.string

  let of_source source =
    let open Lwt_result_syntax in
    return (Michelson_v1_parser.parse_toplevel source)

  let to_source ({Michelson_v1_parser.source; _}, _) =
    let open Lwt_result_syntax in
    return source

  let name = "script"
end)

let print_errors ?parsed (cctxt : #Protocol_client_context.full) errs
    ~show_source =
  let open Lwt_result_syntax in
  let parsed = Option.map Michelson_v1_parser.unrecognize_prims parsed in
  let*! errs =
    Michelson_v1_error_reporter.enrich_runtime_errors
      cctxt
      ~chain:cctxt#chain
      ~block:cctxt#block
      ~parsed
      errs
  in
  let*! () =
    cctxt#warning
      "%a"
      (Michelson_v1_error_reporter.report_errors
         ~details:false
         ?parsed
         ~show_source)
      errs
  in
  let*! () = cctxt#error "error running script" in
  return_unit

let print_view_result (cctxt : #Protocol_client_context.full) =
  let open Lwt_result_syntax in
  function
  | Ok expr ->
      let*! () = cctxt#message "%a" print_expr expr in
      return_unit
  | Error errs -> print_errors cctxt ~show_source:false errs

let print_run_result (cctxt : Protocol_client_context.full) ~show_source ~parsed
    =
  let open Lwt_result_syntax in
  function
  | Ok (storage, operations, maybe_lazy_storage_diff) ->
      let* operations =
        List.map_es
          (Operation_result.normalize_internal_operation cctxt Readable)
          operations
      in
      let*! () =
        cctxt#message
          "@[<v 0>@[<v 2>storage@,\
           %a@]@,\
           @[<v 2>emitted operations@,\
           %a@]@,\
           @[<v 2>big_map diff@,\
           %a@]@]@."
          print_expr
          storage
          (Format.pp_print_list Operation_result.pp_internal_operation)
          operations
          (fun ppf -> function
            | None -> () | Some diff -> print_big_map_diff ppf diff)
          maybe_lazy_storage_diff
      in
      return_unit
  | Error errs -> print_errors cctxt errs ~show_source ~parsed

let print_trace_result (cctxt : #Client_context.printer) ~show_source ~parsed =
  let open Lwt_result_syntax in
  function
  | Ok (storage, operations, trace, maybe_lazy_storage_diff) ->
      let*! () =
        cctxt#message
          "@[<v 0>@[<v 2>storage@,\
           %a@]@,\
           @[<v 2>emitted operations@,\
           %a@]@,\
           @[<v 2>big_map diff@,\
           %a@]@,\
           @[<v 2>trace@,\
           %a@]@]@."
          print_expr
          storage
          (Format.pp_print_list Operation_result.pp_internal_operation)
          operations
          (fun ppf -> function
            | None -> () | Some diff -> print_big_map_diff ppf diff)
          maybe_lazy_storage_diff
          print_execution_trace
          trace
      in
      return_unit
  | Error errs -> print_errors cctxt errs ~show_source ~parsed

let print_run_instr_result (cctxt : #Client_context.printer) ~show_source
    ~parsed =
  let open Lwt_result_syntax in
  function
  | Ok (stack, gas) ->
      let*! () =
        cctxt#message
          "@[<v 0>@[<v 2>Result@,%a@]@,@[<v 2>Gas remaining: %a@]@]@."
          Michelson_v1_printer.print_typed_stack
          stack
          Alpha_context.Gas.pp
          gas
      in
      return_unit
  | Error errs -> print_errors cctxt errs ~show_source ~parsed

type simulation_params = {
  input : Michelson_v1_parser.parsed;
  unparsing_mode : Script_ir_unparser.unparsing_mode;
  now : Script_timestamp.t option;
  level : Script_int.n Script_int.num option;
  sender : Contract.t option;
  payer : Signature.public_key_hash option;
  gas : Gas.Arith.integral option;
  other_contracts : RPC.Scripts.S.other_contract_description list option;
  extra_big_maps : RPC.Scripts.S.extra_big_map_description list option;
}

type run_view_params = {
  shared_params : simulation_params;
  contract : Contract_hash.t;
  entrypoint : Entrypoint.t;
}

type run_script_view_params = {
  shared_params : simulation_params;
  contract : Contract_hash.t;
  view : string;
  unlimited_gas : bool;
}

type run_params = {
  shared_params : simulation_params;
  amount : Tez.t option;
  balance : Tez.t option;
  program : Michelson_v1_parser.parsed;
  storage : Michelson_v1_parser.parsed;
  entrypoint : Entrypoint.t option;
  self : Contract_hash.t option;
}

type run_instr_params = {
  shared_params : simulation_params;
  amount : Tez.t;
  balance : Tez.t option;
  stack : (Script.expr * Script.expr) list;
  self : Contract_hash.t option;
  parameter : Script.expr option;
  legacy : bool;
}

let run_view (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block (params : run_view_params) =
  let open Lwt_result_syntax in
  let {
    shared_params =
      {
        input;
        unparsing_mode;
        now;
        level;
        sender;
        payer;
        gas;
        other_contracts;
        extra_big_maps;
      };
    contract;
    entrypoint;
  } =
    params
  in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  Plugin.RPC.Scripts.run_tzip4_view
    cctxt
    (chain, block)
    ~gas
    ~contract
    ~entrypoint
    ~input:input.expanded
    ~chain_id
    ~sender
    ~payer
    ~unparsing_mode
    ~now
    ~level
    ~other_contracts
    ~extra_big_maps

let run_script_view (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block (params : run_script_view_params) =
  let open Lwt_result_syntax in
  let {
    shared_params =
      {
        input;
        unparsing_mode;
        now;
        level;
        sender;
        payer;
        gas;
        other_contracts;
        extra_big_maps;
      };
    contract;
    view;
    unlimited_gas;
  } =
    params
  in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  Plugin.RPC.Scripts.run_script_view
    cctxt
    (chain, block)
    ~gas
    ~contract
    ~view
    ~input:input.expanded
    ~unlimited_gas
    ~chain_id
    ~sender
    ~payer
    ~unparsing_mode
    ~now
    ~level
    ~other_contracts
    ~extra_big_maps

let run (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block (params : run_params) =
  let open Lwt_result_syntax in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  let {
    shared_params =
      {
        input;
        unparsing_mode;
        now;
        level;
        sender;
        payer;
        gas;
        other_contracts;
        extra_big_maps;
      };
    program;
    amount;
    balance;
    storage;
    entrypoint;
    self;
  } =
    params
  in
  let amount = Option.value ~default:Tez.fifty_cents amount in
  let entrypoint = Option.value ~default:Entrypoint.default entrypoint in
  Plugin.RPC.Scripts.run_code
    cctxt
    (chain, block)
    ~gas
    ~entrypoint
    ~unparsing_mode:(Some unparsing_mode)
    ~script:program.expanded
    ~storage:storage.expanded
    ~input:input.expanded
    ~amount
    ~balance
    ~chain_id
    ~sender
    ~payer
    ~self
    ~now
    ~level
    ~other_contracts
    ~extra_big_maps

let trace (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block (params : run_params) =
  let open Lwt_result_syntax in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  let {
    shared_params =
      {
        input;
        unparsing_mode;
        now;
        level;
        sender;
        payer;
        gas;
        other_contracts;
        extra_big_maps;
      };
    program;
    amount;
    balance;
    storage;
    entrypoint;
    self;
  } =
    params
  in
  let amount = Option.value ~default:Tez.fifty_cents amount in
  let entrypoint = Option.value ~default:Entrypoint.default entrypoint in
  Plugin.RPC.Scripts.trace_code
    cctxt
    (chain, block)
    ~gas
    ~entrypoint
    ~unparsing_mode:(Some unparsing_mode)
    ~script:program.expanded
    ~storage:storage.expanded
    ~input:input.expanded
    ~amount
    ~balance
    ~chain_id
    ~sender
    ~payer
    ~self
    ~now
    ~level
    ~other_contracts
    ~extra_big_maps

let run_instr (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block (params : run_instr_params) =
  let open Lwt_result_syntax in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  let {shared_params; amount; balance; stack; self; parameter; legacy} =
    params
  in
  let {
    input;
    unparsing_mode;
    now;
    level;
    sender;
    payer;
    gas;
    other_contracts;
    extra_big_maps;
  } =
    shared_params
  in
  Plugin.RPC.Scripts.run_instr
    ~gas
    ~legacy
    ~input:stack
    ~code:input.expanded
    ~chain_id
    ~now
    ~level
    ~unparsing_mode:(Some unparsing_mode)
    ~source:payer
    ~sender
    ~self
    ~parameter
    ~amount
    ~balance
    ~other_contracts
    ~extra_big_maps
    cctxt
    (chain, block)

let typecheck_data cctxt ~(chain : Chain_services.chain) ~block ~gas ~legacy
    ~(data : Michelson_v1_parser.parsed) ~(ty : Michelson_v1_parser.parsed) () =
  Plugin.RPC.Scripts.typecheck_data
    cctxt
    (chain, block)
    ~gas
    ~legacy
    ~data:data.expanded
    ~ty:ty.expanded

let typecheck_program cctxt ~(chain : Chain_services.chain) ~block ~gas ~legacy
    ~show_types (program : Michelson_v1_parser.parsed) =
  Plugin.RPC.Scripts.typecheck_code
    cctxt
    (chain, block)
    ~gas
    ~legacy
    ~script:program.expanded
    ~show_types

let script_size cctxt ~(chain : Chain_services.chain) ~block ~gas ~legacy
    ~(program : Michelson_v1_parser.parsed)
    ~(storage : Michelson_v1_parser.parsed) () =
  Plugin.RPC.Scripts.script_size
    cctxt
    (chain, block)
    ~gas
    ~legacy
    ~script:program.expanded
    ~storage:storage.expanded

let print_typecheck_result ~emacs ~show_types ~print_source_on_error
    ~display_names ~name program res (cctxt : #Client_context.printer) =
  let open Lwt_result_syntax in
  if emacs then
    let type_map, errs, _gas =
      match res with
      | Ok (type_map, gas) -> (type_map, [], Some gas)
      | Error
          (Environment.Ecoproto_error
             (Script_tc_errors.Ill_typed_contract (_, type_map))
           :: _ as errs) ->
          (type_map, errs, None)
      | Error errs -> ([], errs, None)
    in
    let*! () =
      cctxt#message
        "(@[<v 0>(types . %a)@ (errors . %a)@])"
        Michelson_v1_emacs.print_type_map
        (program, type_map)
        Michelson_v1_emacs.report_errors
        (program, errs)
    in
    return_unit
  else
    match res with
    | Ok (type_map, gas) ->
        let program = Michelson_v1_printer.inject_types type_map program in
        let*! () =
          cctxt#message
            "@[Well typed (Gas remaining: %a)\t%t@]"
            Gas.pp
            gas
            (fun fmt -> if display_names then Format.pp_print_string fmt name)
        in
        if show_types then
          let*! () = cctxt#message "%a" Micheline_printer.print_expr program in
          return_unit
        else return_unit
    | Error errs ->
        let*! () =
          cctxt#warning
            "%a"
            (Michelson_v1_error_reporter.report_errors
               ~details:show_types
               ~show_source:print_source_on_error
               ~parsed:(Michelson_v1_parser.unrecognize_prims program))
            errs
        in
        cctxt#error "script %S is ill-typed" name

let entrypoint_type cctxt ~(chain : Chain_services.chain) ~block
    (program : Michelson_v1_parser.parsed) ~entrypoint =
  Michelson_v1_entrypoints.script_entrypoint_type
    cctxt
    ~chain
    ~block
    program.expanded
    ~entrypoint

let print_entrypoint_type (cctxt : #Client_context.printer) ~emacs ?script_name
    ~show_source ~parsed ~entrypoint ty =
  Michelson_v1_entrypoints.print_entrypoint_type
    cctxt
    ~entrypoint
    ~emacs
    ?script_name
    ~on_errors:(print_errors cctxt ~show_source ~parsed)
    ty

let list_entrypoints cctxt ~(chain : Chain_services.chain) ~block
    (program : Michelson_v1_parser.parsed) =
  Michelson_v1_entrypoints.list_entrypoints cctxt ~chain ~block program.expanded

let print_entrypoints_list (cctxt : #Client_context.printer) ~emacs ?script_name
    ~show_source ~parsed ty =
  Michelson_v1_entrypoints.print_entrypoints_list
    cctxt
    ~emacs
    ?script_name
    ~on_errors:(print_errors cctxt ~show_source ~parsed)
    ty

let list_unreachables cctxt ~(chain : Chain_services.chain) ~block
    (program : Michelson_v1_parser.parsed) =
  Michelson_v1_entrypoints.list_unreachables
    cctxt
    ~chain
    ~block
    program.expanded

let print_unreachables (cctxt : #Client_context.printer) ~emacs ?script_name
    ~show_source ~parsed ty =
  Michelson_v1_entrypoints.print_unreachables
    cctxt
    ~emacs
    ?script_name
    ~on_errors:(print_errors cctxt ~show_source ~parsed)
    ty
