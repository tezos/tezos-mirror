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
  else Alcotest.failf "Unexpected error: %s" loc

let location = function
  | Environment.Micheline.Prim (loc, _, _, _)
  | Int (loc, _)
  | String (loc, _)
  | Bytes (loc, _)
  | Seq (loc, _) ->
      loc

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
              {
                untyped = untyped_storage_type;
                typed = Script_typed_ir.Ty_ex_c storage_type;
              } )) =
    Script_native_types.Internal_for_tests.types_of_kind kind
  in
  let*?@ ctxt =
    test_unparse_ty "parameter" ctxt untyped_parameter_type parameter_type
  in
  let*? ctxt = test_parse_ty ctxt untyped_parameter_type parameter_type in
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
  let*@ contract_hash =
    Contract.Internal_for_tests.get_clst_contract_hash ctxt
  in
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
  let*@ contract_hash =
    Contract.Internal_for_tests.get_clst_contract_hash ctxt
  in
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
    (test_parse_contract Script.CLST Script_native_types.CLST_kind) ;
  register_test
    ~title:"Check executing native contract"
    (test_call_native_contract
       Script.CLST
       (strip_location unit_param)
       (strip_location unit_param))
