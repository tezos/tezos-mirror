(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Baker, Forge Worker
   Invocation:   dune exec src/proto_alpha/lib_delegate/test/workers/main.exe \
                  -- --file test_forge_worker.ml
   Subject:      Unit tests for the forge worker>

   These tests verify the thread safety properties of the forge worker's
   internal queue management. The forge worker uses:
   - A synchronous get_or_create_queue function (safe in Lwt's cooperative model)
   - Per-delegate signing queues that serialize operations for each delegate
   - Lwt_stream for task queuing

   Note: The get_or_create_queue function is synchronous and therefore
   thread-safe in Lwt's cooperative concurrency model - there are no yield
   points where context switches could occur.
*)

module Internal = Forge_worker.Internal_for_tests

(** Create test delegates from mockup bootstrap accounts.
    Returns up to [n] delegates (max 5). *)
let make_test_delegates n =
  let open Lwt_syntax in
  let* bootstrap_secrets_result =
    Tezos_mockup_commands.Mockup_wallet.default_bootstrap_accounts
  in
  let bootstrap_secrets =
    match bootstrap_secrets_result with
    | Ok secrets -> secrets
    | Error _ -> assert false
  in
  let accounts = Mockup.Protocol_parameters.default_value.bootstrap_accounts in
  let accounts_with_secrets =
    List.combine_drop (List.take_n n accounts) bootstrap_secrets
  in
  let keys =
    Stdlib.List.map Mockup_simulator.make_baking_delegate accounts_with_secrets
  in
  let delegates =
    Stdlib.List.map
      Baking_state_types.Internal_for_tests.make_delegate_from_key
      keys
  in
  Lwt.return delegates

(** {2 Unit Tests for Forge Worker Thread Safety} *)

(**
   Test 1: Synchronous get_or_create_queue is safe

   This test verifies that the synchronous get_or_create_queue function
   correctly handles concurrent calls. Since the function is synchronous
   (no Lwt operations), it cannot be interrupted and is inherently safe.

   Expected behavior: Only ONE queue should exist after concurrent calls.
*)
let test_synchronous_get_or_create_is_safe () =
  let open Lwt_syntax in
  let num_concurrent_calls = 100 in
  let state = Internal.create_test_state () in
  let* delegates = make_test_delegates 1 in
  let delegate = match delegates with [d] -> d | _ -> assert false in
  (* Track all returned queues *)
  let queues_returned = Array.make num_concurrent_calls None in
  (* Launch concurrent get_or_create_queue calls *)
  let make_task i () =
    let queue = Internal.get_or_create_queue state delegate in
    queues_returned.(i) <- Some queue ;
    return_unit
  in
  let tasks = Array.to_list (Array.init num_concurrent_calls make_task) in
  (* Run all tasks concurrently *)
  let* () = Lwt.join (Stdlib.List.map (fun f -> f ()) tasks) in
  (* Verify results *)
  let queue_count = Internal.queue_count state in
  (* There should be exactly 1 queue - the function is synchronous and safe *)
  if queue_count <> 1 then
    Test.fail
      ~__LOC__
      "Expected exactly 1 queue (synchronous function should be safe), but \
       found %d"
      queue_count ;
  (* All returned queues should be physically equal (same instance) *)
  let first_queue = queues_returned.(0) in
  let all_same =
    Array.for_all
      (fun q ->
        match (first_queue, q) with Some q1, Some q2 -> q1 == q2 | _ -> false)
      queues_returned
  in
  if not all_same then
    Test.fail ~__LOC__ "All calls should return the same queue instance" ;
  return_unit

(**
   Test 2: Multiple delegates get separate queues

   This test verifies that different delegates correctly get different queues.
*)
let test_multiple_delegates_get_separate_queues () =
  let open Lwt_syntax in
  (* We can only use up to 5 bootstrap accounts *)
  let num_delegates = 5 in
  let state = Internal.create_test_state () in
  let* delegates = make_test_delegates num_delegates in
  let delegates_array = Array.of_list delegates in
  (* Create a queue for each delegate *)
  let tasks =
    Array.to_list
      (Array.map
         (fun delegate () ->
           let _queue = Internal.get_or_create_queue state delegate in
           return_unit)
         delegates_array)
  in
  let* () = Lwt.join (Stdlib.List.map (fun f -> f ()) tasks) in
  (* Verify exactly num_delegates queues exist *)
  let queue_count = Internal.queue_count state in
  if queue_count <> num_delegates then
    Test.fail
      ~__LOC__
      "Expected %d queues for %d delegates, but found %d"
      num_delegates
      num_delegates
      queue_count ;
  (* Verify each delegate has a queue *)
  Array.iter
    (fun delegate ->
      if not (Internal.has_queue state delegate) then
        Test.fail ~__LOC__ "Delegate missing queue after get_or_create_queue")
    delegates_array ;
  return_unit

(**
   Test 3: Queue reuse for same delegate

   This test verifies that repeated calls for the same delegate
   return the same queue instance (queue reuse).
*)
let test_queue_reuse_for_same_delegate () =
  let open Lwt_syntax in
  let state = Internal.create_test_state () in
  let* delegates = make_test_delegates 1 in
  let delegate = match delegates with [d] -> d | _ -> assert false in
  (* Get queue multiple times *)
  let queue1 = Internal.get_or_create_queue state delegate in
  let queue2 = Internal.get_or_create_queue state delegate in
  let queue3 = Internal.get_or_create_queue state delegate in
  (* All should be the same instance *)
  if not (queue1 == queue2 && queue2 == queue3) then
    Test.fail ~__LOC__ "Expected same queue instance for repeated calls" ;
  (* Only one queue should exist *)
  let queue_count = Internal.queue_count state in
  if queue_count <> 1 then
    Test.fail ~__LOC__ "Expected exactly 1 queue, found %d" queue_count ;
  return_unit

(**
   Test 4: Mixed concurrent access pattern

   This test simulates realistic usage where multiple delegates are being
   accessed concurrently, with some overlap.
*)
let test_mixed_concurrent_access () =
  let open Lwt_syntax in
  let num_unique_delegates = 5 in
  let num_total_calls = 200 in
  let state = Internal.create_test_state () in
  let* delegates = make_test_delegates num_unique_delegates in
  let delegates_array = Array.of_list delegates in
  (* Random access pattern *)
  Random.init 42 ;
  let make_task _ =
    let delegate = delegates_array.(Random.int num_unique_delegates) in
    fun () ->
      let _queue = Internal.get_or_create_queue state delegate in
      return_unit
  in
  let tasks = Array.to_list (Array.init num_total_calls make_task) in
  let* () = Lwt.join (Stdlib.List.map (fun f -> f ()) tasks) in
  (* Verify exactly num_unique_delegates queues exist *)
  let queue_count = Internal.queue_count state in
  if queue_count <> num_unique_delegates then
    Test.fail
      ~__LOC__
      "Expected %d queues for %d unique delegates, but found %d"
      num_unique_delegates
      num_unique_delegates
      queue_count ;
  return_unit

(** {2 Test Registration} *)

let () =
  let t = lazy (Tezt_sink.activate ()) in
  let proto_name =
    String.lowercase_ascii Protocol.name
    |> String.map (function '-' -> '_' | x -> x)
  in
  let register_test (title, test) =
    Test.register
      ~__FILE__
      ~title
      ~tags:[proto_name; "baker"; "thread_safety"; "forge_worker"; "unit"]
    @@ fun () ->
    let open Lwt_syntax in
    let* () = Lazy.force t in
    test ()
  in
  List.iter
    register_test
    [
      ( proto_name ^ ": forge worker - synchronous get_or_create is safe",
        test_synchronous_get_or_create_is_safe );
      ( proto_name ^ ": forge worker - multiple delegates get separate queues",
        test_multiple_delegates_get_separate_queues );
      ( proto_name ^ ": forge worker - queue reuse for same delegate",
        test_queue_reuse_for_same_delegate );
      ( proto_name ^ ": forge worker - mixed concurrent access",
        test_mixed_concurrent_access );
    ]
