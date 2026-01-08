(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trilitech.com>                      *)
(*                                                                           *)
(*****************************************************************************)

module Api = Octez_riscv_api

let test_status () =
  let open Lwt_syntax in
  assert (Api.octez_riscv_test_status Api.Evaluating = Api.Evaluating) ;
  assert (
    Api.octez_riscv_test_status Api.Waiting_for_input = Api.Waiting_for_input) ;
  assert (
    Api.octez_riscv_test_status Api.Waiting_for_reveal = Api.Waiting_for_reveal) ;

  return_unit

let test_input () =
  let open Lwt_syntax in
  let inbox_message_input : Api.input =
    Api.Inbox_message
      {
        inbox_level = 12l;
        message_counter = 42L;
        payload = String.to_bytes "inbox_payload";
      }
  in
  let reveal_input = Api.Reveal (String.to_bytes "reveal_payload") in

  assert (Api.octez_riscv_test_input inbox_message_input = inbox_message_input) ;
  assert (Api.octez_riscv_test_input reveal_input = reveal_input) ;

  return_unit

let test_input_request () =
  let open Lwt_syntax in
  let no_input_required = Api.No_input_required in
  let initial = Api.Initial in
  let first_after = Api.First_after {level = 12l; counter = 42L} in
  let needs_reveal = Api.Needs_reveal (String.to_bytes "test_needs_reveal") in

  assert (
    Api.octez_riscv_test_input_request no_input_required = no_input_required) ;
  assert (Api.octez_riscv_test_input_request initial = initial) ;
  assert (Api.octez_riscv_test_input_request first_after = first_after) ;
  assert (Api.octez_riscv_test_input_request needs_reveal = needs_reveal) ;

  return_unit

let test_output_info () =
  let open Lwt_syntax in
  let output_info = {Api.message_index = 42L; outbox_level = 12l} in

  assert (Api.octez_riscv_test_output_info output_info = output_info) ;

  return_unit

let test_output () =
  let open Lwt_syntax in
  let output_info = {Api.message_index = 42L; outbox_level = 12l} in
  let output =
    {Api.info = output_info; encoded_message = String.to_bytes "test_output"}
  in
  let output_repr = Api.octez_riscv_test_output output in
  assert (output_repr = output) ;

  return_unit
