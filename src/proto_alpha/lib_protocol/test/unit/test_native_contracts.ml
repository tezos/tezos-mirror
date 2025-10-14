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
    Script_native.Internal_for_tests.types_of_kind kind
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
    (test_native_contract_types Script_native_repr.Accumulator)
