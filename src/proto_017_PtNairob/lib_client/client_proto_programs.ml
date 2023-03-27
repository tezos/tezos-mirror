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

  let of_source source = return (Michelson_v1_parser.parse_toplevel source)

  let to_source ({Michelson_v1_parser.source; _}, _) = return source

  let name = "script"
end)

let print_errors ?parsed (cctxt : #Protocol_client_context.full) errs
    ~show_source =
  Michelson_v1_error_reporter.enrich_runtime_errors
    cctxt
    ~chain:cctxt#chain
    ~block:cctxt#block
    ~parsed
    errs
  >>= fun errs ->
  cctxt#warning
    "%a"
    (Michelson_v1_error_reporter.report_errors
       ~details:false
       ?parsed
       ~show_source)
    errs
  >>= fun () ->
  cctxt#error "error running script" >>= fun () -> return_unit

let print_view_result (cctxt : #Protocol_client_context.full) = function
  | Ok expr -> cctxt#message "%a" print_expr expr >>= fun () -> return_unit
  | Error errs -> print_errors cctxt ~show_source:false errs

let print_run_result (cctxt : #Client_context.printer) ~show_source ~parsed =
  function
  | Ok (storage, operations, maybe_lazy_storage_diff) ->
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
          | None -> ()
          | Some diff -> print_big_map_diff ppf diff)
        maybe_lazy_storage_diff
      >>= fun () -> return_unit
  | Error errs -> print_errors cctxt errs ~show_source ~parsed

let print_trace_result (cctxt : #Client_context.printer) ~show_source ~parsed =
  function
  | Ok (storage, operations, trace, maybe_lazy_storage_diff) ->
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
          | None -> ()
          | Some diff -> print_big_map_diff ppf diff)
        maybe_lazy_storage_diff
        print_execution_trace
        trace
      >>= fun () -> return_unit
  | Error errs -> print_errors cctxt errs ~show_source ~parsed

type simulation_params = {
  input : Michelson_v1_parser.parsed;
  unparsing_mode : Script_ir_unparser.unparsing_mode;
  now : Script_timestamp.t option;
  level : Script_int.n Script_int.num option;
  source : Contract.t option;
  payer : Signature.public_key_hash option;
  gas : Gas.Arith.integral option;
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

let run_view (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block (params : run_view_params) =
  let {
    shared_params = {input; unparsing_mode; now; level; source; payer; gas};
    contract;
    entrypoint;
  } =
    params
  in
  Chain_services.chain_id cctxt ~chain () >>=? fun chain_id ->
  Plugin.RPC.Scripts.run_tzip4_view
    cctxt
    (chain, block)
    ?gas
    ~contract
    ~entrypoint
    ~input:input.expanded
    ~chain_id
    ?source
    ?payer
    ~unparsing_mode
    ~now
    ~level

let run_script_view (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block (params : run_script_view_params) =
  let {
    shared_params = {input; unparsing_mode; now; level; source; payer; gas};
    contract;
    view;
    unlimited_gas;
  } =
    params
  in
  Chain_services.chain_id cctxt ~chain () >>=? fun chain_id ->
  Plugin.RPC.Scripts.run_script_view
    cctxt
    (chain, block)
    ?gas
    ~contract
    ~view
    ~input:input.expanded
    ~unlimited_gas
    ~chain_id
    ?source
    ?payer
    ~unparsing_mode
    ~now
    ~level

let run (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block (params : run_params) =
  Chain_services.chain_id cctxt ~chain () >>=? fun chain_id ->
  let {
    shared_params = {input; unparsing_mode; now; level; source; payer; gas};
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
  Plugin.RPC.Scripts.run_code
    cctxt
    (chain, block)
    ?gas
    ?entrypoint
    ~unparsing_mode
    ~script:program.expanded
    ~storage:storage.expanded
    ~input:input.expanded
    ~amount
    ?balance
    ~chain_id
    ~source
    ~payer
    ~self
    ~now
    ~level

let trace (cctxt : #Protocol_client_context.rpc_context)
    ~(chain : Chain_services.chain) ~block (params : run_params) =
  Chain_services.chain_id cctxt ~chain () >>=? fun chain_id ->
  let {
    shared_params = {input; unparsing_mode; now; level; source; payer; gas};
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
  Plugin.RPC.Scripts.trace_code
    cctxt
    (chain, block)
    ?gas
    ?entrypoint
    ~unparsing_mode
    ~script:program.expanded
    ~storage:storage.expanded
    ~input:input.expanded
    ~amount
    ?balance
    ~chain_id
    ~source
    ~payer
    ~self
    ~now
    ~level

let typecheck_data cctxt ~(chain : Chain_services.chain) ~block ?gas ?legacy
    ~(data : Michelson_v1_parser.parsed) ~(ty : Michelson_v1_parser.parsed) () =
  Plugin.RPC.Scripts.typecheck_data
    cctxt
    (chain, block)
    ?gas
    ?legacy
    ~data:data.expanded
    ~ty:ty.expanded

let typecheck_program cctxt ~(chain : Chain_services.chain) ~block ?gas ?legacy
    ~show_types (program : Michelson_v1_parser.parsed) =
  Plugin.RPC.Scripts.typecheck_code
    cctxt
    (chain, block)
    ?gas
    ?legacy
    ~script:program.expanded
    ~show_types

let script_size cctxt ~(chain : Chain_services.chain) ~block ?gas ?legacy
    ~(program : Michelson_v1_parser.parsed)
    ~(storage : Michelson_v1_parser.parsed) () =
  Plugin.RPC.Scripts.script_size
    cctxt
    (chain, block)
    ?gas
    ?legacy
    ~script:program.expanded
    ~storage:storage.expanded

let print_typecheck_result ~emacs ~show_types ~print_source_on_error ~name
    program res (cctxt : #Client_context.printer) =
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
    cctxt#message
      "(@[<v 0>(types . %a)@ (errors . %a)@])"
      Michelson_v1_emacs.print_type_map
      (program, type_map)
      Michelson_v1_emacs.report_errors
      (program, errs)
    >>= fun () -> return_unit
  else
    match res with
    | Ok (type_map, gas) ->
        let program = Michelson_v1_printer.inject_types type_map program in
        cctxt#message
          "@[<v 0>%aWell typed@,Gas remaining: %a@]"
          (fun fmt filename ->
            match filename with
            | None -> ()
            | Some filename -> Format.fprintf fmt "%s@," filename)
          name
          Gas.pp
          gas
        >>= fun () ->
        if show_types then
          cctxt#message "%a" Micheline_printer.print_expr program >>= fun () ->
          return_unit
        else return_unit
    | Error errs ->
        cctxt#warning
          "%a"
          (Michelson_v1_error_reporter.report_errors
             ~details:show_types
             ~show_source:print_source_on_error
             ~parsed:program)
          errs
        >>= fun () -> cctxt#error "ill-typed script"

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
