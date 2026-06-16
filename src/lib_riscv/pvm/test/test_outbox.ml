(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    RISC-V PVM
    Invocation:   dune exec src/lib_riscv/pvm/test/main.exe
    Subject:      Outbox proof production and verification using the echo kernel
*)

open Octez_riscv_pvm

(* Run the PVM until it requests input. *)
let run_until_input_requested state max_steps =
  let open Lwt_syntax in
  let* _steps = Backend.Mutable_state.compute_step_many ~max_steps state in
  let* status = Backend.Mutable_state.get_status state in
  if status <> Waiting_for_input then
    Alcotest.fail
      (Format.asprintf
         "Expected kernel to wait for input after at most %Ld steps"
         max_steps) ;
  return_unit

(* Set up the echo kernel and run until first input is requested. *)
let setup_echo_kernel () =
  let open Lwt_syntax in
  let state = Storage.empty () in
  let* kernel = Utils.read_riscv_echo_kernel () in
  let* () = Backend.Mutable_state.install_boot_sector state kernel in
  let* () = run_until_input_requested state 10000L in
  return state

(* Feed external messages at the given inbox level *)
let feed_inbox_messages ~level ~messages state =
  let open Lwt_syntax in
  Lwt_list.iteri_s
    (fun i msg ->
      (* Tag as an external message *)
      let msg = "\x01" ^ msg in
      let* () =
        Backend.Mutable_state.set_input
          state
          (Inbox_message (level, Int64.of_int i, msg))
      in
      run_until_input_requested state 10000L)
    messages

let produce_proof imm_state ~level ~index =
  let outbox_level =
    match Bounded.Non_negative_int32.of_value level with
    | Some level -> level
    | None -> Alcotest.fail "Invalid outbox level"
  in
  let output_info = Backend.{message_index = Z.of_int index; outbox_level} in
  Backend.produce_output_proof imm_state output_info

(* Attempt to produce an output proof and assert that it succeeds *)
let assert_produce_proof state ~level ~index =
  match produce_proof state ~level ~index with
  | Ok proof -> proof
  | Error err -> Alcotest.fail ("Failed to produce output proof: " ^ err)

(* Attempt to produce an output proof and assert that it fails *)
let assert_produce_proof_fails state ~level ~index =
  match produce_proof state ~level ~index with
  | Ok _ ->
      Alcotest.fail
        (Format.asprintf
           "Expected proof production to fail for level %ld, index=%d"
           level
           index)
  | Error _ -> ()

let test_output_proof_serialisation_round_trip () =
  let open Lwt_syntax in
  let* state = setup_echo_kernel () in
  let message = "round-trip test" in
  let* () = feed_inbox_messages ~level:0l ~messages:[message] state in

  let imm_state = Backend.Mutable_state.to_imm state in
  let proof = assert_produce_proof imm_state ~level:0l ~index:0 in
  let serialised = Backend.serialise_output_proof proof in
  match Backend.deserialise_output_proof serialised with
  | Error err -> Lwt.fail_with ("Output proof deserialisation failed: " ^ err)
  | Ok deserialised_proof -> (
      match Backend.verify_output_proof_res deserialised_proof with
      | Error err ->
          Lwt.fail_with ("Deserialised output proof verification failed: " ^ err)
      | Ok output ->
          assert (output.encoded_message = message) ;
          assert (Z.equal output.info.message_index Z.zero) ;
          assert (
            Bounded.Non_negative_int32.to_value output.info.outbox_level = 0l) ;
          return_unit)

let test_produce_output_proofs () =
  let open Lwt_syntax in
  let* state = setup_echo_kernel () in
  let messages = ["first"; "second"; "third"] in
  let* () = feed_inbox_messages ~level:0l ~messages state in
  let imm_state = Backend.Mutable_state.to_imm state in
  Lwt_list.iteri_s
    (fun i expected_msg ->
      let proof = assert_produce_proof imm_state ~level:0l ~index:i in
      let state_hash = Backend.state_hash imm_state in
      let output_info = Backend.output_info_of_output_proof proof in
      assert (Z.equal output_info.message_index (Z.of_int i)) ;
      assert (Bounded.Non_negative_int32.to_value output_info.outbox_level = 0l) ;
      match Backend.verify_output_proof_res proof with
      | Error err ->
          Lwt.fail_with
            (Format.asprintf
               "Output proof verification failed for message %d: %s"
               i
               err)
      | Ok output ->
          assert (output.encoded_message = expected_msg) ;
          assert (Z.equal output.info.message_index (Z.of_int i)) ;
          assert (Backend.state_of_output_proof proof = state_hash) ;
          return_unit)
    messages

let test_produce_proof_invalid_message_index () =
  let open Lwt_syntax in
  let* state = setup_echo_kernel () in
  let* () = feed_inbox_messages ~level:0l ~messages:["hello"] state in
  let imm_state = Backend.Mutable_state.to_imm state in
  assert_produce_proof_fails imm_state ~level:0l ~index:1 ;
  assert_produce_proof_fails imm_state ~level:0l ~index:99 ;
  return_unit

let test_produce_proof_invalid_outbox_level () =
  let open Lwt_syntax in
  let* state = setup_echo_kernel () in
  let* () = feed_inbox_messages ~level:0l ~messages:["hello"] state in
  let imm_state = Backend.Mutable_state.to_imm state in
  assert_produce_proof_fails imm_state ~level:1l ~index:0 ;
  assert_produce_proof_fails imm_state ~level:999l ~index:0 ;
  return_unit

let test_produce_proof_empty_outbox () =
  let open Lwt_syntax in
  let* state = setup_echo_kernel () in
  let imm_state = Backend.Mutable_state.to_imm state in
  assert_produce_proof_fails imm_state ~level:0l ~index:0 ;
  return_unit

let test_deserialise_invalid_bytes () =
  let open Lwt_syntax in
  (match Backend.deserialise_output_proof (Bytes.of_string "") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Deserialising empty bytes should fail") ;
  (match Backend.deserialise_output_proof (Bytes.of_string "not a proof") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Deserialising invalid bytes should fail") ;
  return_unit
