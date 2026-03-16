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
   Subject:      Unit and integration tests for the forge worker

   Unit tests verify the thread safety properties of the forge worker's
   internal queue management. The forge worker uses:
   - A synchronous get_or_create_queue function (safe in Lwt's cooperative model)
   - Per-delegate signing queues that serialize operations for each delegate
   - Lwt_stream for task queuing

   Integration tests verify the actual forging behavior by:
   - Starting a forge worker with Forge_worker.start
   - Pushing requests with Forge_worker.push_request
   - Verifying events from Forge_worker.get_event_stream

   Note: The get_or_create_queue function is synchronous and therefore
   thread-safe in Lwt's cooperative concurrency model - there are no yield
   points where context switches could occur.
*)

open Protocol
open Alpha_context
module Internal = Forge_worker.Internal_for_tests

(** Helper to create a slot from int, defaulting to Slot.zero on error *)
let slot_of_int i =
  match Slot.of_int i with Ok slot -> slot | Error _ -> Slot.zero

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
  Lwt.return keys

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
  let* keys = make_test_delegates 1 in
  let delegate =
    match keys with
    | [k] -> Baking_state_types.Internal_for_tests.make_delegate_from_key k
    | _ -> assert false
  in
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
  let* keys = make_test_delegates num_delegates in
  let delegates =
    Stdlib.List.map
      Baking_state_types.Internal_for_tests.make_delegate_from_key
      keys
  in
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
  let* keys = make_test_delegates 1 in
  let delegate =
    match keys with
    | [k] -> Baking_state_types.Internal_for_tests.make_delegate_from_key k
    | _ -> assert false
  in
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
  let* keys = make_test_delegates num_unique_delegates in
  let delegates =
    Stdlib.List.map
      Baking_state_types.Internal_for_tests.make_delegate_from_key
      keys
  in
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

(** {2 Integration Tests using Forge Worker} *)

(** Helper to set up test infrastructure for forge worker tests *)
let with_forge_worker_test_env ?forge_consensus_vote_hook f =
  let open Lwt_result_syntax in
  (* Create baker test environment - reuses same setup as baker_process/run *)
  let* Mockup_simulator.{cctxt = _; delegates; global_state; genesis_block_info}
      =
    Mockup_simulator.create_baker_test_env ~num_delegates:3 ()
  in
  (* Start forge worker - with optional hook if provided *)
  let* worker =
    Forge_worker.Internal_for_tests.start
      ?forge_consensus_vote_hook
      global_state
  in
  (* Get event stream *)
  let event_stream = Forge_worker.get_event_stream worker in
  (* Run the test function *)
  let* result =
    f ~worker ~event_stream ~delegates ~block_info:genesis_block_info
  in
  (* Shutdown worker *)
  let*! () = Forge_worker.shutdown worker in
  return result

(** {2 Event Collection Helpers} *)

(** Run [f] with a timeout. Returns the result of [f] or [on_timeout] if timeout expires. *)
let with_timeout ~timeout ~on_timeout f =
  Lwt.pick
    [
      f ();
      (let open Lwt_syntax in
       let* () = Lwt_unix.sleep timeout in
       return on_timeout);
    ]

(** Collect events from stream until [expected] events matching [matches] are received.
    Returns the number of matching events collected (may be less than expected if stream ends). *)
let collect_n_events ~event_stream ~matches ~expected =
  let open Lwt_syntax in
  let count = ref 0 in
  let rec loop () =
    let* event = Lwt_stream.get event_stream in
    match event with
    | Some e when matches e ->
        incr count ;
        if !count >= expected then return !count else loop ()
    | Some _ -> loop ()
    | None -> return !count
  in
  loop ()

(**
   Integration Test 1: Forge and sign attestations

   This test verifies that pushing an attestation request results in
   Attestation_ready events being emitted on the event stream.
*)
let test_forge_attestations () =
  with_forge_worker_test_env
    (fun ~worker ~event_stream ~delegates ~block_info ->
      let open Lwt_result_syntax in
      (* Create attestation batch for all delegates *)
      let batch_content = Mockup_simulator.create_batch_content ~block_info in
      let delegates_with_slots =
        Stdlib.List.mapi
          (fun i key ->
            let delegate =
              Baking_state_types.Internal_for_tests.make_delegate_from_key key
            in
            let slot = slot_of_int i in
            (delegate, slot))
          delegates
      in
      let unsigned_attestations =
        Baking_state.make_unsigned_consensus_vote_batch
          Baking_state.Attestation
          batch_content
          ~batch_branch:block_info.hash
          delegates_with_slots
      in
      (* Push attestation request *)
      let*! pushed =
        Forge_worker.push_request
          worker
          (Baking_state.Forge_and_sign_attestations {unsigned_attestations})
      in
      if not pushed then failwith "Failed to push attestation request"
      else
        (* Wait for attestation events *)
        let is_attestation_ready = function
          | Baking_state.Attestation_ready _ -> true
          | _ -> false
        in
        let expected = List.length delegates in
        let*! received =
          with_timeout ~timeout:5.0 ~on_timeout:0 (fun () ->
              collect_n_events
                ~event_stream
                ~matches:is_attestation_ready
                ~expected)
        in
        if received < expected then
          failwith
            "Expected %d Attestation_ready events, got %d"
            expected
            received
        else return_unit)

(**
   Integration Test 2: Forge and sign preattestations

   This test verifies that pushing a preattestation request results in
   Preattestation_ready events being emitted on the event stream.
*)
let test_forge_preattestations () =
  with_forge_worker_test_env
    (fun ~worker ~event_stream ~delegates ~block_info ->
      let open Lwt_result_syntax in
      (* Create preattestation batch for all delegates *)
      let batch_content = Mockup_simulator.create_batch_content ~block_info in
      let delegates_with_slots =
        Stdlib.List.mapi
          (fun i key ->
            let delegate =
              Baking_state_types.Internal_for_tests.make_delegate_from_key key
            in
            let slot = slot_of_int i in
            (delegate, slot))
          delegates
      in
      let unsigned_preattestations =
        Baking_state.make_unsigned_consensus_vote_batch
          Baking_state.Preattestation
          batch_content
          ~batch_branch:block_info.hash
          delegates_with_slots
      in
      (* Push preattestation request *)
      let*! pushed =
        Forge_worker.push_request
          worker
          (Baking_state.Forge_and_sign_preattestations
             {unsigned_preattestations})
      in
      if not pushed then failwith "Failed to push preattestation request"
      else
        (* Wait for preattestation events *)
        let is_preattestation_ready = function
          | Baking_state.Preattestation_ready _ -> true
          | _ -> false
        in
        let expected = List.length delegates in
        let*! received =
          with_timeout ~timeout:5.0 ~on_timeout:0 (fun () ->
              collect_n_events
                ~event_stream
                ~matches:is_preattestation_ready
                ~expected)
        in
        if received < expected then
          failwith
            "Expected %d Preattestation_ready events, got %d"
            expected
            received
        else return_unit)

(**
   Integration Test 3: Forge and sign block

   This test verifies that pushing a block forging request results in
   a Block_ready event being emitted on the event stream.
*)
let test_forge_block () =
  with_forge_worker_test_env
    (fun ~worker ~event_stream ~delegates ~block_info ->
      let open Lwt_result_syntax in
      match delegates with
      | [] -> failwith "No delegates available"
      | key :: _ ->
          let delegate =
            Baking_state_types.Internal_for_tests.make_delegate_from_key key
          in
          (* Create block_to_bake *)
          let block_to_bake =
            Baking_state.
              {
                predecessor = block_info;
                round = Round.zero;
                delegate;
                kind = Fresh Operation_pool.empty;
                force_apply = false;
              }
          in
          (* Push block forging request *)
          let*! pushed =
            Forge_worker.push_request
              worker
              (Baking_state.Forge_and_sign_block block_to_bake)
          in
          if not pushed then failwith "Failed to push block forging request"
          else
            (* Wait for block ready event *)
            let block_ready = ref false in
            let timeout = 10.0 in
            let*! () =
              Lwt.pick
                [
                  (let rec collect () =
                     let open Lwt_syntax in
                     let* event = Lwt_stream.get event_stream in
                     match event with
                     | Some (Baking_state.Block_ready _) ->
                         block_ready := true ;
                         return_unit
                     | Some _ -> collect ()
                     | None -> return_unit
                   in
                   collect ());
                  (let open Lwt_syntax in
                   let* () = Lwt_unix.sleep timeout in
                   return_unit);
                ]
            in
            if not !block_ready then
              failwith "Did not receive Block_ready event within timeout"
            else return_unit)

(**
   Integration Test 4: Multiple delegates produce events concurrently

   This test verifies that when multiple delegates push requests,
   events are produced for all of them.
*)
let test_multiple_delegates_concurrent () =
  with_forge_worker_test_env
    (fun ~worker ~event_stream ~delegates ~block_info ->
      let open Lwt_result_syntax in
      (* Create attestation requests for each delegate separately *)
      let batch_content = Mockup_simulator.create_batch_content ~block_info in
      (* Push requests for each delegate *)
      let*! () =
        Lwt_list.iter_s
          (fun key ->
            let delegate =
              Baking_state_types.Internal_for_tests.make_delegate_from_key key
            in
            let slot = slot_of_int 0 in
            let unsigned_attestations =
              Baking_state.make_unsigned_consensus_vote_batch
                Baking_state.Attestation
                batch_content
                ~batch_branch:block_info.hash
                [(delegate, slot)]
            in
            let open Lwt_syntax in
            let* _pushed =
              Forge_worker.push_request
                worker
                (Baking_state.Forge_and_sign_attestations
                   {unsigned_attestations})
            in
            return_unit)
          delegates
      in
      (* Collect events from different delegates *)
      let delegates_seen = Stdlib.Hashtbl.create 5 in
      let timeout = 5.0 in
      let*! () =
        Lwt.pick
          [
            (let rec collect () =
               let open Lwt_syntax in
               let* event = Lwt_stream.get event_stream in
               match event with
               | Some (Baking_state.Attestation_ready signed_vote) -> (
                   let delegate_id =
                     Baking_state_types.Delegate.delegate_id
                       signed_vote.unsigned_consensus_vote.delegate
                   in
                   match Stdlib.Hashtbl.find_opt delegates_seen delegate_id with
                   | Some () ->
                       Lwt.fail_with
                         "Duplicate Attestation_ready event for same delegate"
                   | None ->
                       Stdlib.Hashtbl.add delegates_seen delegate_id () ;
                       if
                         Stdlib.Hashtbl.length delegates_seen
                         >= List.length delegates
                       then return_unit
                       else collect ())
               | Some _ -> collect ()
               | None -> return_unit
             in
             collect ());
            (let open Lwt_syntax in
             let* () = Lwt_unix.sleep timeout in
             return_unit);
          ]
      in
      let num_delegates_seen = Stdlib.Hashtbl.length delegates_seen in
      if num_delegates_seen < List.length delegates then
        failwith
          "Expected events from %d delegates, got %d"
          (List.length delegates)
          num_delegates_seen
      else return_unit)

(**
   Integration Test 5: Cancel pending tasks

   This test verifies that cancel_all_pending_tasks stops pending
   tasks from being processed. Uses a 5-second delay hook to ensure
   tasks remain pending long enough to be cancelled.
*)
let test_cancel_pending_tasks () =
  with_forge_worker_test_env
    ~forge_consensus_vote_hook:(fun () -> Lwt_unix.sleep 1.0)
    (fun ~worker ~event_stream ~delegates ~block_info ->
      let open Lwt_result_syntax in
      let key = Stdlib.List.hd delegates in
      let delegate =
        Baking_state_types.Internal_for_tests.make_delegate_from_key key
      in
      let slot = slot_of_int 0 in
      (* Push many attestation requests rapidly *)
      let round = ref 0 in
      let push_requests num_requests =
        let*! () =
          Lwt_list.iter_s
            (fun _ ->
              let block_info =
                {
                  block_info with
                  round =
                    (match Round.of_int !round with
                    | Ok x -> x
                    | _ -> assert false);
                }
              in
              incr round ;
              let batch_content =
                Mockup_simulator.create_batch_content ~block_info
              in
              let unsigned_attestations =
                Baking_state.make_unsigned_consensus_vote_batch
                  Baking_state.Attestation
                  batch_content
                  ~batch_branch:block_info.hash
                  [(delegate, slot)]
              in
              let open Lwt_syntax in
              let* _pushed =
                Forge_worker.push_request
                  worker
                  (Baking_state.Forge_and_sign_attestations
                     {unsigned_attestations})
              in
              return_unit)
            (Array.to_list (Array.init num_requests Fun.id))
        in
        return_unit
      in
      let collect_events timeout =
        let events_received = ref 0 in
        let*! () =
          Lwt.pick
            [
              (let rec collect () =
                 let open Lwt_syntax in
                 let* event = Lwt_stream.get event_stream in
                 match event with
                 | Some (Baking_state.Attestation_ready _) ->
                     incr events_received ;
                     collect ()
                 | Some _ -> collect ()
                 | None -> return_unit
               in
               collect ());
              (let open Lwt_syntax in
               let* () = Lwt_unix.sleep timeout in
               return_unit);
            ]
        in
        return !events_received
      in

      (* Push 3 requests, each request takes around 1sec to complete. *)
      let* () = push_requests 3 in
      (* Takes 4 seconds to collect the events, we should see 3 events. *)
      let* events = collect_events 4. in
      let* () =
        if events != 3 then failwith "Expected 3 events, got %d" events
        else return_unit
      in

      (* Push 3 requests *)
      let* () = push_requests 3 in
      (* Sleeps to let the forge worker push the tasks in the delegate queue's task. *)
      let*! () = Lwt_unix.sleep 0.5 in
      Forge_worker.cancel_all_pending_tasks worker ;
      (* Takes 6 seconds to collect the events, we should not see 3 events. *)
      let* events = collect_events 6. in
      let* () =
        if events >= 3 then
          failwith "Expected less than 3 events, got %d" events
        else return_unit
      in
      return_unit)

(**
   Integration Test 6: Multiple delegates requests are processed

   This test verifies that requests for multiple delegates are processed
   correctly, with each delegate's requests being serialized.

   Note: Watermark protection prevents the same delegate from signing
   the same level/round multiple times, so we use different delegates.
*)
let test_same_delegate_serialization () =
  with_forge_worker_test_env
    (fun ~worker ~event_stream ~delegates ~block_info ->
      let open Lwt_result_syntax in
      let batch_content = Mockup_simulator.create_batch_content ~block_info in
      (* Push one request per delegate - each request contains one attestation *)
      let num_requests = List.length delegates in
      let*! () =
        Lwt_list.iteri_s
          (fun i key ->
            let delegate =
              Baking_state_types.Internal_for_tests.make_delegate_from_key key
            in
            let slot = slot_of_int i in
            let unsigned_attestations =
              Baking_state.make_unsigned_consensus_vote_batch
                Baking_state.Attestation
                batch_content
                ~batch_branch:block_info.hash
                [(delegate, slot)]
            in
            let open Lwt_syntax in
            let* _pushed =
              Forge_worker.push_request
                worker
                (Baking_state.Forge_and_sign_attestations
                   {unsigned_attestations})
            in
            return_unit)
          delegates
      in
      (* Collect events and verify they arrive - one per delegate *)
      let is_attestation_ready = function
        | Baking_state.Attestation_ready _ -> true
        | _ -> false
      in
      let expected = num_requests in
      let*! received =
        with_timeout ~timeout:5.0 ~on_timeout:0 (fun () ->
            collect_n_events
              ~event_stream
              ~matches:is_attestation_ready
              ~expected)
      in
      if received < expected then
        failwith
          "Expected %d events for serialized requests, got %d"
          expected
          received
      else return_unit)

(** {2 Test Registration} *)

let () =
  let t = lazy (Tezt_sink.activate ()) in
  let proto_name =
    String.lowercase_ascii Protocol.name
    |> String.map (function '-' -> '_' | x -> x)
  in
  (* Register unit tests *)
  let register_unit_test (title, test) =
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
    register_unit_test
    [
      ( proto_name ^ ": forge worker - synchronous get_or_create is safe",
        test_synchronous_get_or_create_is_safe );
      ( proto_name ^ ": forge worker - multiple delegates get separate queues",
        test_multiple_delegates_get_separate_queues );
      ( proto_name ^ ": forge worker - queue reuse for same delegate",
        test_queue_reuse_for_same_delegate );
      ( proto_name ^ ": forge worker - mixed concurrent access",
        test_mixed_concurrent_access );
    ] ;
  (* Register integration tests *)
  let register_integration_test (title, test) =
    Test.register
      ~__FILE__
      ~title
      ~tags:[proto_name; "baker"; "forge_worker"; "integration"]
    @@ fun () ->
    let open Lwt_result_syntax in
    let*! () = Lazy.force t in
    let*! result = test () in
    match result with
    | Ok () -> Lwt.return_unit
    | Error errs -> Test.fail ~__LOC__ "%a" Error_monad.pp_print_trace errs
  in
  List.iter
    register_integration_test
    [
      ( proto_name ^ ": forge worker integration - attestations",
        test_forge_attestations );
      ( proto_name ^ ": forge worker integration - preattestations",
        test_forge_preattestations );
      (proto_name ^ ": forge worker integration - block", test_forge_block);
      ( proto_name ^ ": forge worker integration - multiple delegates",
        test_multiple_delegates_concurrent );
      ( proto_name ^ ": forge worker integration - cancel pending",
        test_cancel_pending_tasks );
      ( proto_name ^ ": forge worker integration - serialization",
        test_same_delegate_serialization );
    ]
