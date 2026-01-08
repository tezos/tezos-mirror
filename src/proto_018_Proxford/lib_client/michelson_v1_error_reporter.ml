(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Script_tc_errors
open Script_interpreter
open Michelson_v1_printer

let print_ty ppf ty = Michelson_v1_printer.print_expr_unwrapped ppf ty

let print_stack_ty ?(depth = max_int) ppf s =
  let rec loop depth ppf = function
    | [] -> ()
    | _ when depth <= 0 -> Format.fprintf ppf "..."
    | [last] -> print_ty ppf last
    | last :: rest ->
        Format.fprintf ppf "%a@ :@ %a" print_ty last (loop (depth - 1)) rest
  in
  match s with
  | [] -> Format.fprintf ppf "[]"
  | sty -> Format.fprintf ppf "@[<hov 2>[ %a ]@]" (loop depth) sty

let rec print_enumeration ppf = function
  | [single] -> Format.fprintf ppf "%a" Format.pp_print_text single
  | [prev; last] ->
      Format.fprintf
        ppf
        "%a@ or@ %a"
        Format.pp_print_text
        prev
        Format.pp_print_text
        last
  | first :: rest ->
      Format.fprintf
        ppf
        "%a,@ %a"
        Format.pp_print_text
        first
        print_enumeration
        rest
  | [] -> assert false

let collect_error_locations errs =
  let rec collect acc = function
    | Environment.Ecoproto_error
        ( Ill_formed_type (_, _, _)
        | No_such_entrypoint _ | Duplicate_entrypoint _
        | Unreachable_entrypoint _ | Tx_rollup_invalid_ticket_amount _
        | Runtime_contract_error _
        | Michelson_v1_primitives.Invalid_primitive_name (_, _)
        | Ill_typed_data (_, _, _)
        | Ill_typed_contract (_, _) )
      :: _
    | [] ->
        acc
    | Environment.Ecoproto_error
        ( Invalid_arity (loc, _, _, _)
        | Invalid_seq_arity (loc, _, _)
        | Unexpected_annotation loc
        | Ungrouped_annotations loc
        | Type_too_large (loc, _)
        | Invalid_namespace (loc, _, _, _)
        | Invalid_primitive (loc, _, _)
        | Invalid_kind (loc, _, _)
        | Invalid_never_expr loc
        | Duplicate_field (loc, _)
        | Unexpected_lazy_storage loc
        | Unexpected_operation loc
        | Fail_not_in_tail_position loc
        | Undefined_binop (loc, _, _, _)
        | Undefined_unop (loc, _, _)
        | Bad_return (loc, _, _)
        | Bad_stack (loc, _, _, _)
        | Unmatched_branches (loc, _, _)
        | Forbidden_instr_in_context (loc, _, _)
        | Invalid_constant (loc, _, _)
        | Invalid_syntactic_constant (loc, _, _)
        | Invalid_contract (loc, _)
        | Comparable_type_expected (loc, _)
        | Overflow (loc, _)
        | Reject (loc, _, _)
        | Pair_bad_argument loc
        | Unpair_bad_argument loc
        | Dup_n_bad_argument loc )
      :: rest ->
        collect (loc :: acc) rest
    | _ :: rest -> collect acc rest
  in
  collect [] errs

let string_of_context_desc = function
  | Script_tc_errors.Lambda -> "lambda"
  | Script_tc_errors.View -> "view"

(* Error raised while fetching the script of a contract for error reporting when the script is not found. *)
type error += Fetch_script_not_found_meta_error of Contract_hash.t

(* Errors raised while fetching the script of a contract for error reporting. *)
type error += Fetch_script_meta_error of error trace

let fetch_script (cctxt : #Protocol_client_context.rpc_context) ~chain ~block
    contract =
  let open Lwt_result_syntax in
  let* script_opt =
    Plugin.RPC.Contract.get_script_normalized
      cctxt
      (chain, block)
      ~unparsing_mode:Readable
      ~normalize_types:true
      ~contract
  in
  match script_opt with
  | None -> tzfail (Fetch_script_not_found_meta_error contract)
  | Some {code; storage = _} ->
      Lwt.return @@ Environment.wrap_tzresult @@ Script_repr.force_decode code

type error +=
  | Rich_runtime_contract_error of Contract_hash.t * Michelson_v1_parser.parsed

let enrich_runtime_errors cctxt ~chain ~block ~parsed =
  let open Lwt_result_syntax in
  List.map_s (function
    | Environment.Ecoproto_error (Runtime_contract_error contract) -> (
        (* If we know the script already, we don't fetch it *)
        match parsed with
        | Some parsed ->
            Lwt.return (Rich_runtime_contract_error (contract, parsed))
        | None -> (
            let*! script_opt = fetch_script cctxt ~chain ~block contract in
            Lwt.return
            @@
            match script_opt with
            | Ok script ->
                let parsed = Michelson_v1_printer.unparse_toplevel script in
                Rich_runtime_contract_error (contract, parsed)
            | Error err -> Fetch_script_meta_error err))
    | e -> Lwt.return e)

let report_errors ~details ~show_source ?parsed ppf errs =
  let rec print_trace locations errs =
    let print_loc ppf loc =
      match locations loc with
      | None -> Format.fprintf ppf "At (unshown) location %d, " loc
      | Some loc ->
          Format.fprintf
            ppf
            "%s,@ "
            (String.capitalize_ascii
               (Format.asprintf "%a" Micheline_parser.print_location loc))
    in
    let parsed_locations parsed loc =
      let ( >?? ) = Option.bind in
      List.assoc
        ~equal:Int.equal
        loc
        parsed.Michelson_v1_parser.unexpansion_table
      >?? fun oloc ->
      List.assoc ~equal:Int.equal oloc parsed.expansion_table
      >?? fun (ploc, _) -> Some ploc
    in
    let print_source ppf (parsed, _hilights) (* TODO *) =
      let lines = String.split_on_char '\n' parsed.Michelson_v1_parser.source in
      let cols = String.length (string_of_int (List.length lines)) in
      Format.fprintf
        ppf
        "@[<v 0>%a@]"
        (Format.pp_print_list (fun ppf (i, l) ->
             Format.fprintf ppf "%0*d: %s" cols i l))
        (List.rev_mapi (fun i x -> (i + 1, x)) lines |> List.rev)
    in
    match errs with
    | [] -> ()
    | Michelson_v1_stack.Wrong_stack_item (loc, expr) :: rest ->
        Format.fprintf
          ppf
          "@[<v 0>%s,@ wrong syntax for a stack element, expecting something \
           of the following shape: Stack_elt <ty> <val>; got %a.@]"
          (String.capitalize_ascii
             (Format.asprintf "%a" Micheline_parser.print_location loc))
          Micheline_printer.print_expr_unwrapped
          expr ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Michelson_v1_stack.Wrong_stack (loc, expr) :: rest ->
        Format.fprintf
          ppf
          "@[<v 0>%s,@ wrong syntax for stack, expecting a sequence of \
           elements of the following shape: Stack_elt <ty> <val>; got %a.@]"
          (String.capitalize_ascii
             (Format.asprintf "%a" Micheline_parser.print_location loc))
          Micheline_printer.print_expr_unwrapped
          expr ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error
        (Michelson_v1_primitives.Invalid_primitive_name (expr, loc))
      :: rest ->
        let parsed =
          match parsed with
          | Some parsed ->
              if
                Micheline.strip_locations
                  (Michelson_v1_macros.unexpand_rec (Micheline.root expr))
                = parsed.Michelson_v1_parser.unexpanded
              then parsed
              else Michelson_v1_printer.unparse_invalid expr
          | None -> Michelson_v1_printer.unparse_invalid expr
        in
        let hilights = loc :: collect_error_locations rest in
        if show_source then
          Format.fprintf
            ppf
            "@[<hov 0>@[<hov 2>Invalid primitive:@ %a@]@]"
            print_source
            (parsed, hilights)
        else Format.fprintf ppf "Invalid primitive." ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace (parsed_locations parsed) rest
    | Environment.Ecoproto_error (Ill_typed_data (name, expr, ty)) :: rest ->
        let parsed =
          match parsed with
          | Some parsed when expr = parsed.Michelson_v1_parser.expanded ->
              parsed
          | Some _ | None -> Michelson_v1_printer.unparse_expression expr
        in
        let hilights = collect_error_locations rest in
        Format.fprintf
          ppf
          "@[<hov 0>@[<hov 2>Ill typed %adata:@ %a@]@ @[<hov 2>is not an \
           expression of type@ %a@]@]"
          (fun ppf -> function
            | None -> () | Some s -> Format.fprintf ppf "%s " s)
          name
          print_source
          (parsed, hilights)
          print_ty
          ty ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace (parsed_locations parsed) rest
    | Environment.Ecoproto_error (No_such_entrypoint entrypoint) :: rest ->
        Format.fprintf
          ppf
          "Contract has no entrypoint named %a"
          Entrypoint.pp
          entrypoint ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Duplicate_entrypoint entrypoint) :: rest ->
        Format.fprintf
          ppf
          "Contract has two entrypoints named %a"
          Entrypoint.pp
          entrypoint ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Unreachable_entrypoint path) :: rest ->
        let path =
          String.concat
            "/"
            (List.map Michelson_v1_primitives.string_of_prim path)
        in
        Format.fprintf ppf "Entrypoint at path %s is not reachable" path ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Tx_rollup_bad_deposit_parameter (loc, expr))
      :: rest ->
        Format.fprintf
          ppf
          "@[<v 2>%aTrying to call the deposit entrypoint of a transaction \
           rollup with an ill-formed parameter:@ %a@]"
          print_loc
          loc
          print_expr
          expr ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Tx_rollup_invalid_ticket_amount amount)
      :: rest ->
        Format.fprintf
          ppf
          "Amount of tickets to deposit to a transaction rollup needs to fit \
           in a 64-bit integer, but %a is too big"
          Z.pp_print
          amount ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Ill_formed_type (_, expr, loc)) :: rest ->
        let parsed =
          match parsed with
          | Some parsed when expr = parsed.Michelson_v1_parser.expanded ->
              parsed
          | Some _ | None -> Michelson_v1_printer.unparse_expression expr
        in
        let hilights = loc :: collect_error_locations errs in
        if show_source then
          Format.fprintf
            ppf
            "@[<v 2>%aill formed type:@ %a@]"
            print_loc
            loc
            print_source
            (parsed, hilights)
        else Format.fprintf ppf "Ill formed type." ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace (parsed_locations parsed) rest
    | Environment.Ecoproto_error (Ill_typed_contract (expr, type_map)) :: rest
      ->
        let parsed =
          match parsed with
          | Some parsed
            when (not details) && expr = parsed.Michelson_v1_parser.expanded ->
              parsed
          | Some _ | None ->
              Michelson_v1_printer.unparse_toplevel ~type_map expr
        in
        let hilights = collect_error_locations rest in
        if show_source then
          Format.fprintf
            ppf
            "@[<v 0>Ill typed contract:@,  %a@]"
            print_source
            (parsed, hilights)
        else Format.fprintf ppf "Ill typed contract." ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace (parsed_locations parsed) rest
    | Environment.Ecoproto_error
        Validate_errors.Manager.Gas_quota_exceeded_init_deserialize
      :: rest ->
        Format.fprintf
          ppf
          "@[<v 0>Not enough gas to deserialize the operation.@,\
           Injecting such a transaction could have you banned from mempools.@]" ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Deprecated_instruction prim) :: rest ->
        Format.fprintf
          ppf
          "@[<v 0>Use of deprecated instruction: %s@]"
          (Michelson_v1_primitives.string_of_prim prim) ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error Cannot_serialize_storage :: rest ->
        Format.fprintf
          ppf
          "Cannot serialize the resulting storage value within the provided \
           gas bounds." ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Missing_field prim) :: rest ->
        Format.fprintf
          ppf
          "@[<v 0>Missing contract field: %s@]"
          (Michelson_v1_primitives.string_of_prim prim) ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Duplicate_field (loc, prim)) :: rest ->
        Format.fprintf
          ppf
          "@[<v 0>%aduplicate contract field: %s@]"
          print_loc
          loc
          (Michelson_v1_primitives.string_of_prim prim) ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Unexpected_lazy_storage loc) :: rest ->
        Format.fprintf
          ppf
          "%abig_map or sapling_state type not expected here"
          print_loc
          loc ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Unexpected_operation loc) :: rest ->
        Format.fprintf
          ppf
          "%aoperation type forbidden in parameter, storage and constants"
          print_loc
          loc ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Unexpected_contract loc) :: rest ->
        Format.fprintf
          ppf
          "%acontract type forbidden in storage and constants"
          print_loc
          loc ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error (Runtime_contract_error contract) :: rest ->
        Format.fprintf
          ppf
          "@[<v 2>Runtime error in unknown contract %a@]"
          Contract_hash.pp
          contract ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Rich_runtime_contract_error (contract, parsed) :: rest ->
        let hilights = collect_error_locations rest in
        Format.fprintf
          ppf
          "@[<v 2>Runtime error in contract %a:@ %a@]"
          Contract_hash.pp
          contract
          print_source
          (parsed, hilights) ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace (parsed_locations parsed) rest
    | Environment.Ecoproto_error (Apply.Internal_operation_replay op) :: rest ->
        Format.fprintf
          ppf
          "@[<v 2>Internal operation replay attempt:@,%a@]"
          Operation_result.pp_internal_operation
          op ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error Gas.Gas_limit_too_high :: rest ->
        Format.fprintf
          ppf
          "Gas limit for the operation is out of the protocol hard bounds." ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error Gas.Block_quota_exceeded :: rest ->
        Format.fprintf
          ppf
          "Gas limit for the block exceeded during typechecking or execution." ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error Gas.Operation_quota_exceeded :: rest ->
        Format.fprintf
          ppf
          "@[<v 0>Gas limit exceeded during typechecking or execution.@,\
           Try again with a higher gas limit.@]" ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | Environment.Ecoproto_error Fees.Operation_quota_exceeded :: rest ->
        Format.fprintf
          ppf
          "@[<v 0>Storage limit exceeded during typechecking or execution.@,\
           Try again with a higher storage limit.@]" ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | [Environment.Ecoproto_error (Script_interpreter.Bad_contract_parameter c)]
      ->
        Format.fprintf
          ppf
          "@[<v 0>Account %a is not a smart contract, it does not take \
           arguments.@,\
           The `-arg' flag should not be used when transferring to an \
           account.@]"
          Contract.pp
          c
    | Environment.Ecoproto_error err :: rest ->
        (match err with
        | Bad_contract_parameter c ->
            Format.fprintf
              ppf
              "Invalid argument passed to contract %a."
              Contract.pp
              c
        | Invalid_arity (loc, name, exp, got) ->
            Format.fprintf
              ppf
              "%aprimitive %s expects %d arguments but is given %d."
              print_loc
              loc
              (Michelson_v1_primitives.string_of_prim name)
              exp
              got
        | Invalid_seq_arity (loc, exp, got) ->
            Format.fprintf
              ppf
              "%asequence expects at least %d elements but is given %d."
              print_loc
              loc
              exp
              got
        | Invalid_namespace (loc, name, exp, got) ->
            let human_namespace = function
              | Michelson_v1_primitives.Instr_namespace -> ("an", "instruction")
              | Type_namespace -> ("a", "type name")
              | Constant_namespace -> ("a", "constant constructor")
              | Keyword_namespace -> ("a", "keyword")
              | Constant_hash_namespace -> ("a", "constant hash")
            in
            Format.fprintf
              ppf
              "@[%aunexpected %s %s, only %s %s can be used here."
              print_loc
              loc
              (snd (human_namespace got))
              (Michelson_v1_primitives.string_of_prim name)
              (fst (human_namespace exp))
              (snd (human_namespace exp))
        | Invalid_primitive (loc, exp, got) ->
            Format.fprintf
              ppf
              "@[%ainvalid primitive %s, only %a can be used here."
              print_loc
              loc
              (Michelson_v1_primitives.string_of_prim got)
              print_enumeration
              (List.map Michelson_v1_primitives.string_of_prim exp)
        | Invalid_kind (loc, exp, got) ->
            let human_kind = function
              | Seq_kind -> ("a", "sequence")
              | Prim_kind -> ("a", "primitive")
              | Int_kind -> ("an", "int")
              | String_kind -> ("a", "string")
              | Bytes_kind -> ("a", "byte sequence")
            in
            Format.fprintf
              ppf
              "@[%aunexpected %s, only@ %a@ can be used here."
              print_loc
              loc
              (snd (human_kind got))
              print_enumeration
              (List.map
                 (fun k ->
                   let a, n = human_kind k in
                   a ^ " " ^ n)
                 exp)
        | Invalid_never_expr loc ->
            Format.fprintf
              ppf
              "@[%athis expression should have type never but type never has \
               no inhabitant."
              print_loc
              loc
        | Duplicate_map_keys (_, expr) ->
            Format.fprintf
              ppf
              "@[<v 2>Map literals cannot contain duplicate keys, however a \
               duplicate key was found:@ @[%a@]"
              print_expr
              expr
        | Unordered_map_keys (_, expr) ->
            Format.fprintf
              ppf
              "@[<v 2>Keys in a map literal must be in strictly ascending \
               order, but they were unordered in literal:@ @[%a@]"
              print_expr
              expr
        | Duplicate_set_values (_, expr) ->
            Format.fprintf
              ppf
              "@[<v 2>Set literals cannot contain duplicate values, however a \
               duplicate value was found:@ @[%a@]"
              print_expr
              expr
        | Unordered_set_values (_, expr) ->
            Format.fprintf
              ppf
              "@[<v 2>Values in a set literal must be in strictly ascending \
               order, but they were unordered in literal:@ @[%a@]"
              print_expr
              expr
        | Fail_not_in_tail_position loc ->
            Format.fprintf
              ppf
              "%aThe FAIL instruction must appear in a tail position."
              print_loc
              loc
        | Undefined_binop (loc, name, tya, tyb) ->
            Format.fprintf
              ppf
              "@[<hov 0>@[<hov 2>%aoperator %s is undefined between@ %a@]@ \
               @[<hov 2>and@ %a.@]@]"
              print_loc
              loc
              (Michelson_v1_primitives.string_of_prim name)
              print_ty
              tya
              print_ty
              tyb
        | Undefined_unop (loc, name, ty) ->
            Format.fprintf
              ppf
              "@[<hov 0>@[<hov 2>%aoperator %s is undefined on@ %a@]@]"
              print_loc
              loc
              (Michelson_v1_primitives.string_of_prim name)
              print_ty
              ty
        | Bad_return (loc, got, exp) ->
            Format.fprintf
              ppf
              "@[<v 2>%awrong stack type at end of body:@,\
               - @[<v 0>expected return stack type:@ %a,@]@,\
               - @[<v 0>actual stack type:@ %a.@]@]"
              print_loc
              loc
              (fun ppf -> print_stack_ty ppf)
              [exp]
              (fun ppf -> print_stack_ty ppf)
              got
        | Bad_stack (loc, name, depth, sty) ->
            Format.fprintf
              ppf
              "@[<hov 2>%awrong stack type for instruction %s:@ %a.@]"
              print_loc
              loc
              (Michelson_v1_primitives.string_of_prim name)
              (print_stack_ty ~depth)
              sty
        | Unmatched_branches (loc, sta, stb) ->
            Format.fprintf
              ppf
              "@[<v 2>%atwo branches don't end with the same stack type:@,\
               - @[<hov>first stack type:@ %a,@]@,\
               - @[<hov>other stack type:@ %a.@]@]"
              print_loc
              loc
              (fun ppf -> print_stack_ty ppf)
              sta
              (fun ppf -> print_stack_ty ppf)
              stb
        | Bad_view_name loc ->
            Format.fprintf
              ppf
              "@[<v 2>%athe name of view should be of type string @]"
              print_loc
              loc
        | Duplicated_view_name loc ->
            Format.fprintf
              ppf
              "@[<v 2>%athe name of view in toplevel should be unique @]"
              print_loc
              loc
        | Ill_typed_view {loc; actual; expected} ->
            Format.fprintf
              ppf
              "@[<v 2>%athe return of a view block did not match the expected \
               type.@,\
               - @[<hov>resulted view stack type:@ %a,@]@,\
               - @[<hov>expected view stack type:@ %a.@]@]"
              print_loc
              loc
              (fun ppf -> print_stack_ty ppf)
              actual
              (fun ppf -> print_stack_ty ppf)
              expected
        | View_name_too_long name ->
            Format.fprintf
              ppf
              "@[<v 2> A view name, \"%s\", exceeds the maximum length of 31 \
               characters."
              name
        | Inconsistent_type_sizes (size1, size2) ->
            Format.fprintf
              ppf
              "@[<v 2>The two types have different sizes, the first one is of \
               size %d while the other one is of size %d@]"
              size1
              size2
        | Unexpected_annotation loc ->
            Format.fprintf ppf "@[<v 2>%aunexpected annotation." print_loc loc
        | Ungrouped_annotations loc ->
            Format.fprintf
              ppf
              "@[<v 2>%aAnnotations of the same kind must be grouped."
              print_loc
              loc
        | Type_too_large (loc, maximum_size) ->
            Format.fprintf
              ppf
              "@[<v 2>%atype exceeded maximum type size (%d)."
              print_loc
              loc
              maximum_size
        | Pair_bad_argument loc ->
            Format.fprintf
              ppf
              "%aPAIR expects an argument of at least 2."
              print_loc
              loc
        | Unpair_bad_argument loc ->
            Format.fprintf
              ppf
              "%aUNPAIR expects an argument of at least 2."
              print_loc
              loc
        | Dup_n_bad_argument loc ->
            Format.fprintf
              ppf
              "%aDUP n expects an argument of at least 1 (passed 0)."
              print_loc
              loc
        | Forbidden_instr_in_context (loc, ctxt, prim) ->
            Format.fprintf
              ppf
              "%aThe %s instruction cannot appear in a %s."
              print_loc
              loc
              (Michelson_v1_primitives.string_of_prim prim)
              (string_of_context_desc ctxt)
        | Non_dupable_type (loc, ty) ->
            Format.fprintf
              ppf
              "%atype %a cannot be used here because it is not duplicable. \
               Only duplicable types can be used with the DUP instruction and \
               as view inputs and outputs."
              print_loc
              loc
              print_ty
              ty
        | Unexpected_ticket loc ->
            Format.fprintf
              ppf
              "%aTicket in unauthorized position (type error)."
              print_loc
              loc
        | Bad_stack_length -> Format.fprintf ppf "Bad stack length."
        | Bad_stack_item lvl -> Format.fprintf ppf "Bad stack item %d." lvl
        | Unexpected_forged_value loc ->
            Format.fprintf ppf "%aUnexpected forged value." print_loc loc
        | Invalid_constant (loc, got, exp) ->
            Format.fprintf
              ppf
              "@[<hov 0>@[<hov 2>%avalue@ %a@]@ @[<hov 2>is invalid for type@ \
               %a.@]@]"
              print_loc
              loc
              print_expr
              got
              print_ty
              exp
        | Invalid_syntactic_constant (loc, got, exp) ->
            Format.fprintf
              ppf
              "@[<hov 0>@[<hov 2>%avalue@ %a@]@ @[<hov 2>is invalid, expected@ \
               %s@]@]"
              print_loc
              loc
              print_expr
              got
              exp
        | Invalid_contract (loc, contract) ->
            Format.fprintf
              ppf
              "%ainvalid contract %a."
              print_loc
              loc
              Contract.pp
              contract
        | Comparable_type_expected (loc, ty) ->
            Format.fprintf ppf "%acomparable type expected." print_loc loc ;
            Format.fprintf
              ppf
              "@[<hov 0>@[<hov 2>Type@ %a@]@ is not comparable.@]"
              print_ty
              ty
        | Inconsistent_types (loc, tya, tyb) ->
            Format.fprintf
              ppf
              "@[<hov 0>@[<hov 2>%aType@ %a@]@ @[<hov 2>is not compatible with \
               type@ %a.@]@]"
              print_loc
              loc
              print_ty
              tya
              print_ty
              tyb
        | Reject (loc, v, trace) ->
            Format.fprintf
              ppf
              "%ascript reached FAILWITH instruction@ @[<hov 2>with@ %a@]%a"
              print_loc
              loc
              print_expr
              v
              (fun ppf -> function
                | None -> ()
                | Some trace ->
                    Format.fprintf
                      ppf
                      "@,@[<v 2>trace@,%a@]"
                      print_execution_trace
                      trace)
              trace
        | Overflow (loc, trace) ->
            Format.fprintf
              ppf
              "%aunexpected arithmetic overflow%a"
              print_loc
              loc
              (fun ppf -> function
                | None -> ()
                | Some trace ->
                    Format.fprintf
                      ppf
                      "@,@[<v 2>trace@,%a@]"
                      print_execution_trace
                      trace)
              trace
        | Unexpected_implicit_account_parameters_type (loc, expr) ->
            Format.fprintf
              ppf
              "@[<hov 0>@[<hov 2>%aExpression@ %a@]@ @[<hov 2>is not \
               acceptable as a handle to an implicit account, whose parameters \
               type can only be unit or ticket <ty>.@]@]"
              print_loc
              loc
              print_expr
              expr
        | err -> Format.fprintf ppf "%a" Environment.Error_monad.pp err) ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
    | err :: rest ->
        Format.fprintf ppf "%a" Error_monad.pp err ;
        if rest <> [] then Format.fprintf ppf "@," ;
        print_trace locations rest
  in
  Format.fprintf ppf "@[<v 0>" ;
  print_trace (fun _ -> None) errs ;
  Format.fprintf ppf "@]"
