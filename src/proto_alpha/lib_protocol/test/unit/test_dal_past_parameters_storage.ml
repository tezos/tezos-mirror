(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:  Protocol (dal past parameters storage)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_dal_past_parameters_storage.ml
    Subject:    These unit tests check Dal past parameter storage functions.
*)

open Protocol

let assert_equal_parameters ~__LOC__ =
  let open Constants_parametric_repr in
  let eq x y = x.attestation_lag = y.attestation_lag in
  Assert.equal ~loc:__LOC__ eq "parameters.attestation_lag" (fun fmt x ->
      Format.pp_print_int fmt x.attestation_lag)

let parameters_for_a_level () =
  let open Lwt_result_wrap_syntax in
  let* b, _contracts = Context.init1 () in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in

  let ctxt = Alpha_context.Internal_for_tests.to_raw ctxt in

  let parameters n =
    Constants_parametric_previous_repr.
      {
        feature_enable = true;
        incentives_enable = true;
        number_of_slots = 1;
        attestation_lag = n;
        attestation_threshold = 1;
        cryptobox_parameters =
          Cryptobox.
            {
              redundancy_factor = 1;
              page_size = 1;
              slot_size = 1;
              number_of_shards = 1;
            };
        minimal_participation_ratio = Q.zero;
        rewards_ratio = Q.zero;
        traps_fraction = Q.zero;
      }
  in
  let previous_parameters_to_current previous_parameters =
    let Constants_parametric_previous_repr.
          {
            feature_enable;
            incentives_enable;
            number_of_slots;
            attestation_lag;
            attestation_threshold;
            cryptobox_parameters;
            minimal_participation_ratio;
            rewards_ratio;
            traps_fraction;
          } =
      previous_parameters
    in
    Constants_parametric_repr.
      {
        feature_enable;
        incentives_enable;
        number_of_slots;
        attestation_lag;
        attestation_threshold;
        cryptobox_parameters;
        minimal_participation_ratio;
        rewards_ratio;
        traps_fraction;
      }
  in
  let*@ ctxt =
    Dal_storage.save_parameters
      ctxt
      (parameters 5)
      ~next_protocol_activation:(Raw_level_repr.of_int32_exn 5l)
  in
  let*@ ctxt =
    Dal_storage.save_parameters
      ctxt
      (parameters 10)
      ~next_protocol_activation:(Raw_level_repr.of_int32_exn 10l)
  in
  let*@ ctxt =
    Dal_storage.save_parameters
      ctxt
      (parameters 15)
      ~next_protocol_activation:(Raw_level_repr.of_int32_exn 15l)
  in

  let*@ stored_parameters =
    Dal_storage.parameters ctxt (Raw_level_repr.of_int32_exn 0l)
  in
  let* () =
    assert_equal_parameters
      ~__LOC__
      stored_parameters
      (previous_parameters_to_current @@ parameters 5)
  in

  let*@ stored_parameters =
    Dal_storage.parameters ctxt (Raw_level_repr.of_int32_exn 5l)
  in
  let* () =
    assert_equal_parameters
      ~__LOC__
      stored_parameters
      (previous_parameters_to_current @@ parameters 5)
  in

  let*@ stored_parameters =
    Dal_storage.parameters ctxt (Raw_level_repr.of_int32_exn 10l)
  in
  let* () =
    assert_equal_parameters
      ~__LOC__
      stored_parameters
      (previous_parameters_to_current @@ parameters 10)
  in

  let*@ stored_parameters =
    Dal_storage.parameters ctxt (Raw_level_repr.of_int32_exn 15l)
  in
  let* () =
    assert_equal_parameters
      ~__LOC__
      stored_parameters
      (previous_parameters_to_current @@ parameters 15)
  in

  (* If the level is greater than any saved parameters, whatever are the current
     protocol constants are returned. *)
  let*@ stored_parameters =
    Dal_storage.parameters ctxt (Raw_level_repr.of_int32_exn 20l)
  in
  let current_constants = Raw_context.constants ctxt in
  let* () =
    assert_equal_parameters ~__LOC__ stored_parameters current_constants.dal
  in

  return_unit

let tests =
  [Tztest.tztest "parameters for a level" `Quick parameters_for_a_level]

let () =
  Alcotest_lwt.run
    ~__FILE__
    Protocol.name
    [("dal past parameters storage", tests)]
  |> Lwt_main.run
