(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Protocol (interpretation)
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_native_contracts.ml
    Subject:      Native contracts typechecking and interpretation
*)

open Protocol
open Alpha_context

let test_context () =
  let open Lwt_result_syntax in
  let* b, _cs = Context.init3 () in
  let* v = Incremental.begin_construction b in
  return (Incremental.alpha_ctxt v)

let test_unparse_ty loc ctxt expected ty =
  let open Result_syntax in
  let* actual, ctxt =
    Script_ir_unparser.unparse_ty
      ctxt
      ~loc:Environment.Micheline.dummy_location
      ty
  in
  if actual = expected then Ok ctxt
  else Alcotest.failf "Unexpected error: unparsing %s" loc

let pp_expr fmt x =
  Data_encoding.Json.pp
    fmt
    (Data_encoding.Json.construct Script.expr_encoding x)

let pp_node fmt x =
  let expr, locations = Micheline.extract_locations x in
  let pp_locations =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "; ")
      (fun fmt (n1, n2) -> Format.fprintf fmt "(%d, %d)" n1 n2)
  in
  Format.fprintf fmt "%a@,Locations: %a" pp_expr expr pp_locations locations

(** Folds [pair a1 (pair ... (pair an-1 an))] into [pair a1 ... an]
    except when the pair on the right has an annotation, which would
    be lost.

    This mirrors the behavior of
    {!Script_ir_unparser}.unparse_ty_and_entrypoints_uncarbonated *)
let rec fold_pairs =
  let open Micheline in
  let open Michelson_v1_primitives in
  function
  | Prim (loc, T_pair, [tl; tr], annot) -> (
      let tl = fold_pairs tl in
      let tr = fold_pairs tr in
      match tr with
      | Prim (_, T_pair, ts, []) -> Prim (loc, T_pair, tl :: ts, annot)
      | _ -> Prim (loc, T_pair, [tl; tr], annot))
  | Prim (loc, name, args, annot) ->
      Prim (loc, name, List.map fold_pairs args, annot)
  | Seq (loc, ts) -> Seq (loc, List.map fold_pairs ts)
  | x -> x

let test_unparse_parameter_ty ctxt expected ty entrypoints =
  let open Result_syntax in
  let* actual, ctxt =
    Script_ir_unparser.unparse_parameter_ty
      ctxt
      ~loc:Environment.Micheline.dummy_location
      ty
      ~entrypoints
  in
  let actual = Script.strip_annotations actual in
  let expected = Script.strip_annotations (fold_pairs expected) in
  if actual = expected then Ok ctxt
  else
    Alcotest.failf
      "@[<v 2>Unexpected unparsing at %s:@,\
       @[<v 2>Expected:@,\
       %a@]@,\
       @[<v 2>Actual:@,\
       %a@]],"
      __LOC__
      pp_node
      expected
      pp_node
      actual

let location = function
  | Environment.Micheline.Prim (loc, _, _, _)
  | Int (loc, _)
  | String (loc, _)
  | Bytes (loc, _)
  | Seq (loc, _) ->
      loc

let test_parse_parameter_ty (type exp expc) ctxt node
    (expected : (exp, expc) Script_typed_ir.ty) =
  let open Result_wrap_syntax in
  let@ result =
    let* Ex_parameter_ty_and_entrypoints {arg_type; entrypoints = _}, ctxt =
      Script_ir_translator.parse_parameter_ty_and_entrypoints
        ctxt
        ~legacy:true
        node
    in
    let* eq, ctxt =
      Gas_monad.run ctxt
      @@ Script_ir_translator.ty_eq
           ~error_details:(Informative (location node))
           arg_type
           expected
    in
    let+ Eq = eq in
    ctxt
  in
  result

let test_parse_ty (type exp expc) ctxt node
    (expected : (exp, expc) Script_typed_ir.ty) =
  let open Result_wrap_syntax in
  let legacy = false in
  let allow_lazy_storage = true in
  let allow_operation = true in
  let allow_contract = true in
  let allow_ticket = true in
  let@ result =
    let* Script_typed_ir.Ex_ty actual, ctxt =
      Script_ir_translator.parse_ty
        ctxt
        ~legacy
        ~allow_lazy_storage
        ~allow_operation
        ~allow_contract
        ~allow_ticket
        node
    in
    let* eq, ctxt =
      Gas_monad.run ctxt
      @@ Script_ir_translator.ty_eq
           ~error_details:(Informative (location node))
           actual
           expected
    in
    let+ Eq = eq in
    ctxt
  in
  result

(* This test actually checks that the combinators for building types are valid,
   but we don't want to test combinator per combinator, as types are not built
   dynamically but only once. As long as the final type is correct, let's
   consider the type correct. *)
let test_native_contract_types kind () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = test_context () in
  let*?@ (Ex
            ( {
                untyped = untyped_parameter_type;
                typed = Script_typed_ir.Ty_ex_c parameter_type;
              },
              parameter_entrypoints,
              {
                untyped = untyped_storage_type;
                typed = Script_typed_ir.Ty_ex_c storage_type;
              } )) =
    Script_native_types.Internal_for_tests.types_of_kind kind
  in
  let*?@ ctxt =
    test_unparse_parameter_ty
      ctxt
      untyped_parameter_type
      parameter_type
      parameter_entrypoints
  in
  let*? ctxt =
    test_parse_parameter_ty ctxt untyped_parameter_type parameter_type
  in
  let*?@ ctxt =
    test_unparse_ty "storage" ctxt untyped_storage_type storage_type
  in
  let*? _ctxt = test_parse_ty ctxt untyped_storage_type storage_type in
  return_unit

let get_native_contract ctxt contract_hash kind =
  let open Lwt_result_wrap_syntax in
  let*@ _ctxt, contract = Contract.get_script ctxt contract_hash in
  match contract with
  | Some (Native kind_with_storage) when kind_with_storage.kind = kind ->
      return (kind_with_storage, ctxt)
  | Some (Native _) -> Test.fail "Native contract has the wrong kind"
  | None -> Test.fail "Native contract not found"
  | Some (Script _) -> Test.fail "Contract should be of native kind"

let check_parse_contract ctxt kind_with_storage expected_kind =
  let open Lwt_result_wrap_syntax in
  let allow_forged_tickets_in_storage = true in
  let allow_forged_lazy_storage_id_in_storage = true in
  let*@ Ex_script (Script script), _ =
    Script_ir_translator.parse_script
      ctxt
      ~elab_conf:(Script_ir_translator_config.make ~legacy:true ())
      ~allow_forged_tickets_in_storage
      ~allow_forged_lazy_storage_id_in_storage
      (Native kind_with_storage)
  in
  match script.implementation with
  | Native {kind}
    when Script_native_types.Internal_for_tests.eq_native_kind
           kind
           expected_kind ->
      return_unit
  | Native _ -> Test.fail "Unexpected parsed kind"
  | Lambda _ -> Test.fail "Native contract kind parsed as contract with code"

let test_parse_contract kind expected_kind () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = test_context () in
  let*@ contract_hash = Contract.get_clst_contract_hash ctxt in
  let* kind_with_storage, _ = get_native_contract ctxt contract_hash kind in
  check_parse_contract ctxt kind_with_storage expected_kind

let execute_native_contract ctxt contract_hash kind parameter =
  let open Lwt_result_wrap_syntax in
  let* kind_with_storage, ctxt = get_native_contract ctxt contract_hash kind in
  let*@ res, ctxt =
    Script_interpreter.execute
      ctxt
      Readable
      Contract_helpers.default_step_constants
      ~script:(Script.Native kind_with_storage)
      ~cached_script:None
      ~entrypoint:Entrypoint.default
      ~parameter
      ~internal:false
  in
  return (res, ctxt)

let test_call_native_contract kind parameter expected_storage () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = test_context () in
  let*@ contract_hash = Contract.get_clst_contract_hash ctxt in
  let* res, _ctxt = execute_native_contract ctxt contract_hash kind parameter in
  if res.storage <> expected_storage then Test.fail "Unexpected storage" ;
  return_unit

let unit_param =
  Environment.Micheline.(Prim (dummy_location, Script.D_Unit, [], []))

let strip_location = Environment.Micheline.strip_locations

let register_test ~title ?additional_tags ?slow test =
  let unwrap f () =
    let* res = f () in
    match res with
    | Ok () -> return ()
    | Error e ->
        Stdlib.failwith
          (Format.asprintf "Test failed with %a" Error_monad.pp_print_trace e)
  in
  Tezt_helpers.register_test
    ~__FILE__
    ~file_tags:["native_contracts"]
    ~title
    ?additional_tags
    ?slow
    (unwrap test)

let () =
  register_test
    ~title:"check native contract types"
    (test_native_contract_types Script.CLST) ;
  register_test
    ~title:"Check parsing native contract"
    (test_parse_contract Script.CLST Script_native_types.CLST_kind)
