(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (native_contracts)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/main.exe \
                  -- --file test_native_contracts.ml
*)

open Protocol

let test_context ?(enable_feature = true) () =
  let open Lwt_result_syntax in
  let* b, _cs = Context.init3 ~native_contracts_enable:enable_feature () in
  let* v = Incremental.begin_construction b in
  return (Incremental.alpha_ctxt v)

let test_mainnet_context () =
  let open Lwt_result_syntax in
  let constants = Default_parameters.constants_mainnet in
  let* b, _ = Context.init_with_constants1 constants in
  let* v = Incremental.begin_construction b in
  return (Incremental.alpha_ctxt v)

let test_feature_flag_enabled () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = test_context ~enable_feature:true () in
  let*! hash =
    Alpha_context.Contract.Internal_for_tests.get_clst_contract_hash ctxt
  in
  match hash with
  | Ok _ -> return_unit
  | Error _ ->
      Test.fail
        "Contract hash is missing while it should be originated when the flag \
         is enabled."

let test_feature_flag_disabled () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = test_context ~enable_feature:false () in
  let*!@ hash =
    Alpha_context.Contract.Internal_for_tests.get_clst_contract_hash ctxt
  in
  Assert.proto_error ~loc:__LOC__ hash (function
    | Raw_context.Storage_error (Missing_key (_, Raw_context.Get)) -> true
    | _ -> false)

let test_feature_flag_disabled_on_mainnet () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = test_mainnet_context () in
  let*!@ hash =
    Alpha_context.Contract.Internal_for_tests.get_clst_contract_hash ctxt
  in
  Assert.proto_error ~loc:__LOC__ hash (function
    | Raw_context.Storage_error (Missing_key (_, Raw_context.Get)) -> true
    | _ -> false)

let tests =
  [
    Tztest.tztest
      "Check enabling feature flag allows having native contracts"
      `Quick
      test_feature_flag_enabled;
    Tztest.tztest
      "Check disabling feature flag disallow having native contracts"
      `Quick
      test_feature_flag_disabled;
    Tztest.tztest
      "Check feature flag is disabled on mainnet"
      `Quick
      test_feature_flag_disabled_on_mainnet;
  ]

let () =
  Octez_alcotezt.Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("native_contracts", tests)]
  |> Lwt_main.run
