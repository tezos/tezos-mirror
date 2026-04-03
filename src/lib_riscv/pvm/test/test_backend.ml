(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Octez_riscv_pvm

let max_steps = 10000L

let test_advance_dummy_kernel () =
  let open Lwt_syntax in
  let state = Storage.empty () in
  let* kernel = Utils.read_riscv_kernel Dummy in
  let* () = Backend.Mutable_state.install_boot_sector state kernel in

  (* This relies on the kernel being able to step at least `sum(step_count)` steps
   * without requiring input *)
  let step_counts = [1L; 2L; 10L; 100L; 1000L] in
  Lwt_list.iter_s
    (fun max_steps ->
      let* steps = Backend.Mutable_state.compute_step_many ~max_steps state in
      assert (steps = max_steps) ;
      return_unit)
    step_counts

let test_proof_regression kernel () =
  let open Lwt_syntax in
  let state = Storage.empty () in
  let* kernel_path = Utils.read_riscv_kernel kernel in
  let* () = Backend.Mutable_state.install_boot_sector state kernel_path in

  let state = Backend.Mutable_state.to_imm state in
  match Backend.produce_proof None state with
  | Some proof ->
      let proof_bytes = Backend.serialise_proof proof in
      let expected_proof_bytes =
        Utils.read_riscv_proof_first_step kernel |> String.trim
      in
      assert (proof_bytes = Hex.to_bytes_exn (`Hex expected_proof_bytes)) ;

      let initial_proof_hash = Backend.proof_start_state proof in
      let final_proof_hash = Backend.proof_stop_state proof in

      let initial_hash = Backend.state_hash state in
      let* state = Backend.compute_step state in
      let final_hash = Backend.state_hash state in
      assert (final_hash = final_proof_hash) ;
      assert (initial_hash = initial_proof_hash) ;
      assert (final_hash = final_proof_hash) ;

      (* First step after installing boot sector should not require any input.
         Similarly, the input request generated for the next step should be No_input_required.
         We expect this to happen because right after installing the boot sector the internal PVM machinery
         will spend some computation steps to prepare the execution environment for user kernel execution *)
      let input_request = Backend.verify_proof None proof in
      assert (input_request = Some Backend.No_input_required) ;

      return_unit
  | None -> Lwt.fail_with "Could not produce proof"

let test_proof_immutability kernel () =
  let open Lwt_syntax in
  let state = Storage.empty () in
  let* kernel_path = Utils.read_riscv_kernel kernel in
  let* () = Backend.Mutable_state.install_boot_sector state kernel_path in

  let state = Backend.Mutable_state.to_imm state in
  match Backend.produce_proof None state with
  | Some proof ->
      (* Unlike the proof_regression test, now we first call verify_proof
         to check proof immutability between proof_start_state & verify_proof functions *)
      let input_request = Backend.verify_proof None proof in
      assert (input_request = Some Backend.No_input_required) ;

      let initial_proof_hash = Backend.proof_start_state proof in
      let final_proof_hash = Backend.proof_stop_state proof in

      let initial_hash = Backend.state_hash state in
      let* state = Backend.compute_step state in
      let final_hash = Backend.state_hash state in
      assert (final_hash = final_proof_hash) ;
      assert (initial_hash = initial_proof_hash) ;
      assert (final_hash = final_proof_hash) ;

      (* Verify the proof again to test for immutability *)
      let input_request = Backend.verify_proof None proof in
      assert (input_request = Some Backend.No_input_required) ;

      (* Check the initial and final hash again to test for immutability *)
      let second_initial_proof_hash = Backend.proof_start_state proof in
      let second_final_proof_hash = Backend.proof_stop_state proof in
      assert (initial_proof_hash = second_initial_proof_hash) ;
      assert (final_proof_hash = second_final_proof_hash) ;

      return_unit
  | None -> Lwt.fail_with "Could not produce proof"

let test_init_echo_kernel () =
  let open Lwt_syntax in
  let state = Storage.empty () in
  let* kernel = Utils.read_riscv_echo_kernel () in
  let* () = Backend.Mutable_state.install_boot_sector state kernel in
  return_unit

let test_input_request_proof () =
  let open Lwt_syntax in
  let state = Storage.empty () in
  let* kernel = Utils.read_riscv_echo_kernel () in
  let* () = Backend.Mutable_state.install_boot_sector state kernel in

  (* Step until waiting for input *)
  let* _steps = Backend.Mutable_state.compute_step_many ~max_steps state in

  (* Producing a proof without input should fail at a state which expects input *)
  let imm_state = Backend.Mutable_state.to_imm state in
  assert (Backend.produce_proof None imm_state = None) ;

  (* Produce and verify a proof, check the input request is Initial *)
  let first_input = Backend.Inbox_message (0l, 0L, "") in
  (match Backend.produce_proof (Some first_input) imm_state with
  | Some proof ->
      let input_request = Backend.verify_proof (Some first_input) proof in
      assert (input_request = Some Backend.Initial)
  | None -> Alcotest.fail "Could not produce proof at Initial state") ;

  (* Provide the first inbox message *)
  let* () = Backend.Mutable_state.set_input state first_input in

  (* Step until waiting for input again *)
  let* _steps = Backend.Mutable_state.compute_step_many ~max_steps state in
  let* status = Backend.Mutable_state.get_status state in
  assert (status = Waiting_for_input) ;

  (* Producing a proof without input should fail at a state which expects input *)
  let imm_state = Backend.Mutable_state.to_imm state in
  assert (Backend.produce_proof None imm_state = None) ;

  (* Produce and verify a proof, check the input request is First_after *)
  let second_input = Some (Backend.Inbox_message (0l, 1L, "")) in
  (match Backend.produce_proof second_input imm_state with
  | Some proof ->
      let input_request = Backend.verify_proof second_input proof in
      assert (input_request = Some (Backend.First_after (0l, 0L)))
  | None -> Alcotest.fail "Could not produce proof at First_after state") ;

  return_unit
