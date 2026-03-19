(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Address registry
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_address_registry.ml
    Subject:      Test the account registry storage
*)

open Protocol

let init_context () =
  let open Lwt_result_wrap_syntax in
  let* b, _contracts = Context.init_n 2 () in
  let* inc = Incremental.begin_construction b in
  let ctxt = Incremental.alpha_ctxt inc in
  return ctxt

module Z = struct
  include Z
  include Z.Compare
end

let test_init_context_counter () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = init_context () in
  let*@ first_available_counter =
    Alpha_context.Address_registry.Internal_for_tests.get_counter ctxt
  in
  let initial_address =
    Alpha_context.Destination.Contract (Implicit Signature.Public_key_hash.zero)
  in
  let*@ _ctxt, initial_address_counter =
    Alpha_context.Address_registry.find ctxt initial_address
  in
  if initial_address_counter <> Some Z.zero then
    failwith
      "Initial address %a doesn't have counter 0, but got %a"
      Alpha_context.Destination.pp
      initial_address
      (Format.pp_print_option
         ~none:(fun ppf () -> Format.fprintf ppf "no counter")
         Z.pp_print)
      initial_address_counter
  else if Z.(first_available_counter <> succ zero) then
    failwith "Initial address registry counter should be 1"
  else return_unit

let test_counter_increases ?(initial_counter = 10) () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = init_context () in
  let initial_counter = Z.of_int initial_counter in
  let*@ ctxt =
    Alpha_context.Address_registry.Internal_for_tests.set_counter
      ctxt
      initial_counter
  in
  let*@ Alpha_context.Address_registry.{ctxt; index = _; existed} =
    Alpha_context.Address_registry.add_if_missing
      ctxt
      (Alpha_context.Destination.Sc_rollup
         (Alpha_context.Sc_rollup.Address.of_b58check_exn
            "sr163Lv22CdE8QagCwf48PWDTquk6isQwv57"))
  in
  assert (existed = false) ;
  let*@ next_counter =
    Alpha_context.Address_registry.Internal_for_tests.get_counter ctxt
  in
  if Z.(next_counter <> succ initial_counter) then
    failwith
      "Next counter should be the successor of the initial counter: %s %s"
      (Z.to_string next_counter)
      (Z.to_string initial_counter)
  else return_unit

let test_register_address_empty_context () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = init_context () in
  let*?@ addr =
    Alpha_context.Destination.of_b58check "tz1cxcwwnzENRdhe2Kb8ZdTrdNy4bFNyScx5"
  in
  let*@ addr_counter, _ctxt = Script_address_registry.index ctxt addr in
  if addr_counter <> Script_int.one_n then
    failwith "First registered address should have counter one"
  else return_unit

let generate_address number =
  let seed = Bytes.make 32 '\000' in
  Bytes.iteri (fun i _ -> Bytes.set_int8 seed i number) seed ;
  let pkh, _pk, _sk = Signature.generate_key ~seed () in
  Alpha_context.Destination.Contract (Implicit pkh)

let init_registry ctx number =
  let open Lwt_result_wrap_syntax in
  let rec loop ctx curr_number =
    if curr_number >= number then return ctx
    else
      let addr = generate_address curr_number in
      let*@ _, ctx = Script_address_registry.index ctx addr in
      loop ctx (succ curr_number)
  in
  loop ctx 0

let test_register_address_non_empty_registry ?(number_of_addresses = 10) () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = init_context () in
  let* ctxt = init_registry ctxt number_of_addresses in
  let addr = generate_address (number_of_addresses + 1) in
  let*@ addr_counter, _ctxt = Script_address_registry.index ctxt addr in
  let addr_counter = Script_int.to_zint addr_counter in
  if Z.(addr_counter <> succ (of_int number_of_addresses)) then
    failwith
      "Address should have counter %d, got %a"
      number_of_addresses
      Z.pp_print
      addr_counter
  else return_unit

let test_addresses_are_registered_consecutively ?(number_of_addresses = 10) () =
  let open Lwt_result_wrap_syntax in
  let* ctx = init_context () in
  let rec loop ctx curr_number =
    if curr_number > number_of_addresses then return ctx
    else
      let addr = generate_address curr_number in
      let*@ addr_counter, ctx = Script_address_registry.index ctx addr in
      let addr_counter = Script_int.to_zint addr_counter in
      if Z.(addr_counter <> of_int curr_number) then
        failwith
          "Address should have counter %d, got %a"
          curr_number
          Z.pp_print
          addr_counter
      else loop ctx (curr_number + 1)
  in
  (* The registry is always initialized with an address, so we expect
     registering starting from 1. *)
  let* _ = loop ctx 1 in
  return_unit

let test_register_address_twice () =
  let open Lwt_result_wrap_syntax in
  let* ctxt = init_context () in
  let addr =
    Alpha_context.Destination.Contract (Implicit Signature.Public_key_hash.zero)
  in
  let*@ addr_counter, ctxt = Script_address_registry.index ctxt addr in
  let addr_counter = Script_int.to_zint addr_counter in
  let*@ second_addr_counter, _ctxt = Script_address_registry.index ctxt addr in
  let second_addr_counter = Script_int.to_zint second_addr_counter in
  if Z.(addr_counter <> second_addr_counter) then
    failwith "An address cannot have different counter"
  else return_unit

let tests =
  [
    Tztest.tztest "counter is initially one" `Quick test_init_context_counter;
    Tztest.tztest
      "register address on empty context"
      `Quick
      test_register_address_empty_context;
    Tztest.tztest
      "register address on non empty context"
      `Quick
      test_register_address_non_empty_registry;
    Tztest.tztest "counter increases by one" `Quick test_counter_increases;
    Tztest.tztest
      "addresses are registered with consecutive counter"
      `Quick
      test_addresses_are_registered_consecutively;
    Tztest.tztest "register address twice" `Quick test_register_address_twice;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("account registry storage", tests)]
  |> Lwt_main.run
