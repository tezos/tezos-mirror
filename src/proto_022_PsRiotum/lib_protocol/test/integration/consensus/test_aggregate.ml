(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (aggregate)
    Invocation: dune exec src/proto_022_PsRiotum/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_aggregate.ml
*)

open Protocol

(* Init genesis with 5 accounts including at least 2 BLS *)
let init_genesis_with_some_bls_accounts ?policy ?dal_enable
    ?aggregate_attestation () =
  let open Lwt_result_syntax in
  let*? random_accounts = Account.generate_accounts 3 in
  let*? bls_accounts =
    List.init ~when_negative_length:[] 2 (fun _ ->
        Account.new_account ~algo:Signature.Bls ())
  in
  let bootstrap_accounts =
    Account.make_bootstrap_accounts (random_accounts @ bls_accounts)
  in
  let* genesis =
    Block.genesis
      ?dal_enable
      ?aggregate_attestation
      ~consensus_threshold_size:0
      bootstrap_accounts
  in
  let* b = Block.bake ?policy genesis in
  return (genesis, b)

let aggregate_in_mempool_error = function
  | Validate_errors.Consensus.Aggregate_in_mempool -> true
  | _ -> false

let aggregate_disabled_error = function
  | Validate_errors.Consensus.Aggregate_disabled -> true
  | _ -> false

let aggregate_unimplemented_error = function
  | Validate_errors.Consensus.Aggregate_not_implemented -> true
  | _ -> false

let test_aggregate_feature_flag_enabled () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:true ()
  in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block
    ~application_error:aggregate_unimplemented_error
    ~construction_error:aggregate_unimplemented_error
    ~mempool_error:aggregate_in_mempool_error
    Aggregate

let test_aggregate_feature_flag_disabled () =
  let open Lwt_result_syntax in
  let* _genesis, attested_block =
    init_genesis_with_some_bls_accounts ~aggregate_attestation:false ()
  in
  Consensus_helpers.test_consensus_operation_all_modes_different_outcomes
    ~loc:__LOC__
    ~attested_block
    ~application_error:aggregate_disabled_error
    ~construction_error:aggregate_disabled_error
    ~mempool_error:aggregate_in_mempool_error
    Aggregate

let tests =
  [
    Tztest.tztest
      "test_aggregate_feature_flag_enabled"
      `Quick
      test_aggregate_feature_flag_enabled;
    Tztest.tztest
      "test_aggregate_feature_flag_disabled"
      `Quick
      test_aggregate_feature_flag_disabled;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("aggregate", tests)]
  |> Lwt_main.run
