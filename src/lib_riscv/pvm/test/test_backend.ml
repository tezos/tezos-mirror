(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Octez_riscv_pvm

let test_advance_dummy_kernel () =
  let open Lwt_syntax in
  let state = Storage.empty () in
  let* kernel = Utils.read_riscv_dummy_kernel () in
  let* state = Backend.install_boot_sector state kernel in
  let state = Backend.Mutable_state.from_imm state in

  (* This relies on the kernel being able to step at least `sum(step_count)` steps
   * without requiring input *)
  let step_counts = [1L; 2L; 10L; 100L; 1000L] in
  Lwt_list.iter_s
    (fun max_steps ->
      let* steps = Backend.Mutable_state.compute_step_many ~max_steps state in
      assert (steps = max_steps) ;
      return_unit)
    step_counts

let test_jstz_proof_regression () =
  let open Lwt_syntax in
  let state = Storage.empty () in
  let* kernel = Utils.read_riscv_jstz_kernel () in
  let* state = Backend.install_boot_sector state kernel in

  match Backend.produce_proof None state with
  | Some proof ->
      let proof_bytes = Backend.serialise_proof proof in
      let expected_proof_bytes =
        Utils.read_riscv_jstz_proof_first_step () |> String.trim
      in
      assert (proof_bytes = Hex.to_bytes_exn (`Hex expected_proof_bytes)) ;

      let initial_proof_hash = Backend.proof_start_state proof in
      let final_proof_hash = Backend.proof_stop_state proof in

      let initial_hash = Backend.state_hash state in
      let* state = Backend.compute_step state in
      let final_hash = Backend.state_hash state in

      assert (initial_hash = initial_proof_hash) ;
      assert (final_hash = final_proof_hash) ;
      return_unit
  | None -> Lwt.fail_with "Could not produce proof"
