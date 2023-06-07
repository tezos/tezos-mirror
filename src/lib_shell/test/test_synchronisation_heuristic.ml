(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Shell
   Invocation:   dune exec src/lib_shell/test/main.exe \
                  -- --file test_synchronisation_heuristic.ml
   Subject:      Test the synchronisation heuristic
*)

(* In the following we will use:

   - `Sync` for Synchronised with `is_chain_stuck` is `false`

   - `Stuck` for Synchronised with `is_chain_stuck` is `true`

   - `Unsync` for `Not_synchronised`

   A value refers to a pair for a timestamp and a peer. A value in the
   past means a value which is more than `latency` in the past where
   `latency` is a parameter of the test.

   By default, for each value, we generate a timestamp using the
   current time when the test is executed. Using a `latency` of `100`
   assumes that each unit test takes less than `100` to be
   executed. *)

module Assert = Assert
open Synchronisation_heuristic
open Core

let prn = function
  | Synchronised {is_chain_stuck = true} -> "Synchronised (stuck)"
  | Not_synchronised -> "Not synchronised"
  | Synchronised {is_chain_stuck = false} -> "Synchronised (not stuck)"

let pp ppf state = Format.pp_print_string ppf (prn state)

let forge_peer_id () =
  let identity = P2p_identity.generate_with_pow_target_0 () in
  identity.peer_id

let forge_timestamp ?(delay = 0) () =
  let time = Time.System.to_protocol @@ Time.System.now () in
  Time.Protocol.add time (Int64.of_int delay)

(* NOTE: timestamp supersedes delay *)
let forge_value ?delay ?timestamp ?peer () =
  let peer = match peer with Some peer -> peer | None -> forge_peer_id () in
  let timestamp =
    match timestamp with
    | Some timestamp -> timestamp
    | None -> forge_timestamp ?delay ()
  in
  (timestamp, peer)

(* Test.
   Check the status is `Sync` when the threshold is negative. *)
let test_threshold_negative () =
  let heuristic = create ~threshold:(-1) ~latency:120 in
  Assert.equal ~pp (get_status heuristic) Not_synchronised ;
  update heuristic @@ forge_value () ;
  Assert.equal ~pp (get_status heuristic) Not_synchronised ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  Assert.equal ~pp (get_status heuristic) Not_synchronised

(* Test.
   Check the status is `Sync` when the threshold is zero. *)
let test_threshold_is_zero () =
  let heuristic = create ~threshold:0 ~latency:120 in
  Assert.equal
    ~pp
    (get_status heuristic)
    (Synchronised {is_chain_stuck = false}) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~pp
    (get_status heuristic)
    (Synchronised {is_chain_stuck = false}) ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~pp
    (get_status heuristic)
    (Synchronised {is_chain_stuck = false})

(* Test.

   When the threshold is one, Check that:

   1. The status is first `Unsync`

   2. After adding one or more values, the status is `Sync`
*)
let test_threshold_is_one () =
  let heuristic = create ~threshold:1 ~latency:120 in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is one, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is `Sync`

   3. After adding a value in the past, the status is still `Sync`
*)
let test_threshold_is_one_update_in_the_past () =
  let latency = 120 in
  let heuristic = create ~threshold:1 ~latency in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer = forge_peer_id () in
  update heuristic @@ forge_value ~peer () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) ~peer () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -10) ~peer () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is one,  check that:

   1. The status is `Unsync`

   2. After adding a value in the past, the status is still `Unsync`
*)
let test_threshold_is_one_value_in_the_past () =
  let latency = 120 in
  let heuristic = create ~threshold:1 ~latency in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -10) () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic)

(* Test.

   When the threshold is one, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is `Sync`

   3. After adding a value for the same peer with an old timestamp the
   status is still `Sync` *)
let test_threshold_is_one_always_takes_best_timestamp () =
  let latency = 120 in
  let heuristic = create ~threshold:1 ~latency in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -10) () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the treshold is two, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is still `Unsync`

   3. After adding another value from another peer, the status is
   `Sync`

   4. After adding more values (including in the past, from other peers), the
   status still is `Sync`
*)
let test_threshold_is_two () =
  let latency = 120 in
  let heuristic = create ~threshold:2 ~latency in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer = forge_peer_id () in
  update heuristic @@ forge_value ~peer () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  update heuristic @@ forge_value ~peer ~delay:(latency * -2) () ;
  update heuristic @@ forge_value ~delay:(latency * -10) () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is two, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is still `Unsync`

   3. After adding a value in the past, the status is still `Unsync`

   4. Adding adding a new value value with the same peer as step 3,
   the status is `Sync` *)
let test_threshold_is_two_one_in_the_past () =
  let latency = 120 in
  let heuristic = create ~threshold:2 ~latency in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer = forge_peer_id () in
  update heuristic @@ forge_value ~peer ~delay:(latency * -2) () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~peer () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is two, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is still `Unsync`

   3. After adding a value from the same peer, the status is still `Unsync` *)
let test_threshold_is_two_one_in_the_past_and_one_more () =
  let latency = 120 in
  let heuristic = create ~threshold:2 ~latency in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer = forge_peer_id () in
  update heuristic @@ forge_value ~peer ~delay:(-3) () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~peer ~delay:(-1) () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic)

(* Test.

   When the threshold is two, check that:

   1. The status is `Unsync`

   2. After adding a value in the past, the status is still `Unsync`

   3. After adding another value with the same timestamp but a different peer,
   the status is `Stuck`.

   4. After a more recent value, the status is `Unsync`.
*)
let test_threshold_is_two_two_in_the_past () =
  let latency = 120 in
  let heuristic = create ~threshold:2 ~latency in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let timestamp = forge_timestamp ~delay:(latency * -3) () in
  update heuristic @@ forge_value ~timestamp () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~timestamp () ;
  Assert.equal ~pp (Synchronised {is_chain_stuck = true}) (get_status heuristic) ;
  update heuristic @@ forge_value ~delay:(latency * -2) () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic)

(* Test.

   When the threshold is three, check that:

   1. The status is `Unsync`

   2. After adding a value, the status is still `Unsync`

   3. After adding another value, the status is still `Unsync`

   4. After adding another value, the status is `Sync`
*)
let test_threshold_is_three () =
  let latency = 120 in
  let heuristic = create ~threshold:3 ~latency in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* Test.

   When the threshold is three, check that:

   1. The status is `Unsync`

   2. After adding a value `(t1, peer1)` with t1 in the past, the
   status is still `Unsync`

   3. After adding a value `(t1, peer2)`, the status is still `Unsync`

   4. After adding a value `(t1, peer3)`, the status is `Stuck`

   5. After adding a value `(t2, peer1)`, the status is `Unsync`

   6. After adding a value `(t3, peer2)`, the status is `Unsync`

   7. After adding a value `(t4, peer3)`, the status is `Sync` *)
let test_threshold_is_three_and_stuck () =
  let latency = 120 in
  let heuristic = create ~threshold:3 ~latency in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer1 = forge_peer_id () in
  let timestamp = forge_timestamp ~delay:(latency * -2) () in
  update heuristic @@ forge_value ~peer:peer1 ~timestamp () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer2 = forge_peer_id () in
  update heuristic @@ forge_value ~peer:peer2 ~delay:(latency * -2) () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer3 = forge_peer_id () in
  update heuristic @@ forge_value ~peer:peer3 ~timestamp () ;
  Assert.equal ~pp (Synchronised {is_chain_stuck = true}) (get_status heuristic) ;
  update heuristic @@ forge_value ~peer:peer1 () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~peer:peer2 () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  update heuristic @@ forge_value ~peer:peer3 () ;
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic)

(* This counterexample was generated by crowbar in a previous version
   of the synchronisation heuristic. *)
let test_counterexample_1 () =
  let latency = 100 in
  let heuristic = create ~threshold:3 ~latency in
  let p9 = forge_peer_id () in
  let fresh = forge_peer_id () in
  let p2 = forge_peer_id () in
  let p7 = forge_peer_id () in
  let p8 = forge_peer_id () in
  let delay = -279 in
  let peer = p9 in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -273 in
  let peer = fresh in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -200 in
  let peer = p2 in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -123 in
  let peer = p9 in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -162 in
  let peer = p7 in
  update heuristic @@ forge_value ~peer ~delay () ;
  let delay = -50 in
  let peer = p8 in
  update heuristic @@ forge_value ~peer ~delay () ;
  Assert.equal ~pp Not_synchronised (get_status heuristic)

let tests_raw : (string * (unit -> unit)) list =
  [
    ("Threshold negative", test_threshold_negative);
    ("Threshold is zero", test_threshold_is_zero);
    ("Threshold is one, update in the present", test_threshold_is_one);
    ( "Threshold is one, update in the past",
      test_threshold_is_one_update_in_the_past );
    ( "Threshold is one, best timestamp in the past",
      test_threshold_is_one_value_in_the_past );
    ( "Threshold is one, update in the past does not erase update in the present",
      test_threshold_is_one_always_takes_best_timestamp );
    ("Threshold is two", test_threshold_is_two);
    ( "Threshold is two, one peer in the past",
      test_threshold_is_two_one_in_the_past );
    ( "Threshold is two, one peer in the past twice",
      test_threshold_is_two_one_in_the_past_and_one_more );
    ( "test_threshold is two, two peers in the past",
      test_threshold_is_two_two_in_the_past );
    ("Threshold is three", test_threshold_is_three);
    ("Threshold is three, three in the past", test_threshold_is_three_and_stuck);
    ("Crowbar counterexample 1", test_counterexample_1);
  ]

let tests =
  List.map (fun (s, f) -> Alcotest_lwt.test_case_sync s `Quick f) tests_raw

open Bootstrapping

let test_bp_create () =
  let latency = 100 in
  let called = ref false in
  let when_status_changes _ =
    called := true ;
    Lwt.return_unit
  in
  let _heuristic = create ~when_status_changes ~threshold:3 ~latency () in
  Assert.equal !called false ;
  Lwt.return_unit

let test_bp_activate () =
  let open Lwt_syntax in
  let latency = 100 in
  let called = ref false in
  let when_status_changes _ =
    called := true ;
    Lwt.return_unit
  in
  let heuristic = create ~when_status_changes ~threshold:3 ~latency () in
  let* () = activate heuristic in
  Assert.equal !called true ;
  Lwt.return_unit

let test_bp_create_2 () =
  let latency = 100 in
  let status_changes_called = ref false in
  let bootstrapped_changes_called = ref false in
  let when_status_changes _ =
    status_changes_called := true ;
    Lwt.return_unit
  in
  let when_bootstrapped_changes b =
    Assert.equal b true ;
    bootstrapped_changes_called := true ;
    Lwt.return_unit
  in
  let _heuristic =
    create
      ~when_status_changes
      ~when_bootstrapped_changes
      ~threshold:0
      ~latency
      ()
  in
  Assert.equal !bootstrapped_changes_called false ;
  Assert.equal !status_changes_called false ;
  Lwt.return_unit

let test_bp_activate_2 () =
  let open Lwt_syntax in
  let latency = 100 in
  let status_changes_called = ref false in
  let bootstrapped_changes_called = ref false in
  let when_status_changes _ =
    status_changes_called := true ;
    Lwt.return_unit
  in
  let when_bootstrapped_changes b =
    Assert.equal b true ;
    bootstrapped_changes_called := true ;
    Lwt.return_unit
  in
  let heuristic =
    create
      ~when_status_changes
      ~when_bootstrapped_changes
      ~threshold:0
      ~latency
      ()
  in
  let* () = activate heuristic in
  Assert.equal !status_changes_called true ;
  Assert.equal !bootstrapped_changes_called true ;
  Lwt.return_unit

let test_force_bootstrapped () =
  let open Lwt_syntax in
  let latency = 100 in
  let bootstrapped_changes_called = ref 0 in
  let when_bootstrapped_changes b =
    let expected_value =
      if !bootstrapped_changes_called = 0 then false else true
    in
    Assert.equal b expected_value ;
    incr bootstrapped_changes_called ;
    Lwt.return_unit
  in
  let heuristic = create ~when_bootstrapped_changes ~threshold:1 ~latency () in
  let* () = activate heuristic in
  let* () = force_bootstrapped heuristic true in
  Assert.equal !bootstrapped_changes_called 2 ;
  Lwt.return_unit

let test_force_bootstrapped_2 () =
  let open Lwt_syntax in
  let latency = 100 in
  let bootstrapped_changes_called = ref 0 in
  let when_bootstrapped_changes b =
    Assert.equal b false ;
    incr bootstrapped_changes_called ;
    Lwt.return_unit
  in
  let heuristic = create ~when_bootstrapped_changes ~threshold:1 ~latency () in
  let* () = activate heuristic in
  let* () = force_bootstrapped heuristic false in
  Assert.equal !bootstrapped_changes_called 1 ;
  Lwt.return_unit

let test_force_bootstrapped_3 () =
  let open Lwt_syntax in
  let latency = 100 in
  let bootstrapped_changes_called = ref 0 in
  let when_bootstrapped_changes _b =
    incr bootstrapped_changes_called ;
    Lwt.return_unit
  in
  let heuristic = create ~when_bootstrapped_changes ~threshold:1 ~latency () in
  let* () = activate heuristic in
  let* () = force_bootstrapped heuristic true in
  let* () = force_bootstrapped heuristic false in
  Assert.equal !bootstrapped_changes_called 3 ;
  Lwt.return_unit

let test_is_bootstrapped () =
  let open Lwt_syntax in
  let latency = 100 in
  let heuristic = create ~threshold:1 ~latency () in
  let* () = activate heuristic in
  Assert.equal (is_bootstrapped heuristic) false ;
  Lwt.return_unit

let test_is_bootstrapped_2 () =
  let open Lwt_syntax in
  let latency = 100 in
  let heuristic = create ~threshold:0 ~latency () in
  let* () = activate heuristic in
  Assert.equal (is_bootstrapped heuristic) true ;
  Lwt.return_unit

let test_is_bootstrapped_3 () =
  let open Lwt_syntax in
  let latency = 100 in
  let heuristic = create ~threshold:1 ~latency () in
  let* () = activate heuristic in
  let* () = force_bootstrapped heuristic true in
  Assert.equal (is_bootstrapped heuristic) true ;
  Lwt.return_unit

let test_bootstrapped () =
  let open Lwt_syntax in
  let latency = 100 in
  let heuristic = create ~threshold:0 ~latency () in
  let* () = activate heuristic in
  bootstrapped heuristic

let test_bootstrapped_2 () =
  let open Lwt_syntax in
  let latency = 100 in
  let heuristic = create ~threshold:1 ~latency () in
  let* () = activate heuristic in
  let p = bootstrapped heuristic in
  Assert.equal (Lwt.state p) Lwt.Sleep ;
  let* () = force_bootstrapped heuristic true in
  p

(* A copy of test_threshold_is_three_and_stuck *)
let test_threshold_is_three_and_stuck_with_callbacks () =
  let open Lwt_syntax in
  let latency = 120 in
  let status_changes = ref 0 in
  let when_status_changes status =
    let expected_value =
      if !status_changes = 0 then Not_synchronised
      else if !status_changes = 1 then Synchronised {is_chain_stuck = true}
      else if !status_changes = 2 then Not_synchronised
      else Synchronised {is_chain_stuck = false}
    in
    incr status_changes ;
    Assert.equal status expected_value ;
    Lwt.return_unit
  in
  let heuristic = create ~when_status_changes ~threshold:3 ~latency () in
  let* () = activate heuristic in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer1 = forge_peer_id () in
  let timestamp = forge_timestamp ~delay:(latency * -2) () in
  let* () = update heuristic @@ forge_value ~peer:peer1 ~timestamp () in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer2 = forge_peer_id () in
  let* () =
    update heuristic @@ forge_value ~peer:peer2 ~delay:(latency * -2) ()
  in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let peer3 = forge_peer_id () in
  let* () = update heuristic @@ forge_value ~peer:peer3 ~timestamp () in
  Assert.equal ~pp (Synchronised {is_chain_stuck = true}) (get_status heuristic) ;
  let* () = update heuristic @@ forge_value ~peer:peer1 () in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let* () = update heuristic @@ forge_value ~peer:peer2 () in
  Assert.equal ~pp Not_synchronised (get_status heuristic) ;
  let* () = update heuristic @@ forge_value ~peer:peer3 () in
  Assert.equal
    ~pp
    (Synchronised {is_chain_stuck = false})
    (get_status heuristic) ;
  Assert.equal !status_changes 4 ;
  Lwt.return_unit

let wrap f _switch () = f ()

let tests_lwt_raw : (string * (Lwt_switch.t -> unit -> unit Lwt.t)) list =
  [
    ("test bp create", wrap test_bp_create);
    ("test bp create 2", wrap test_bp_create_2);
    ("test bp activate", wrap test_bp_activate);
    ("test bp activate 2", wrap test_bp_activate_2);
    ("test force bootstrapped", wrap test_force_bootstrapped);
    ("test force bootstrapped 2", wrap test_force_bootstrapped_2);
    ("test force bootstrapped 3", wrap test_force_bootstrapped_3);
    ("test is bootstrapped", wrap test_is_bootstrapped);
    ("test is bootstrapped 2", wrap test_is_bootstrapped_2);
    ("test is bootstrapped 3", wrap test_is_bootstrapped_3);
    ("test waiting for bootstrapped", wrap test_bootstrapped);
    ("test waiting for bootstrapped 2", wrap test_bootstrapped_2);
    ( "test when status changes",
      wrap test_threshold_is_three_and_stuck_with_callbacks );
  ]

let tests_lwt =
  List.map (fun (s, f) -> Alcotest_lwt.test_case s `Quick f) tests_lwt_raw

let () =
  Alcotest_lwt.run
    ~__FILE__
    "tezos-shell"
    [
      ("synchronisation heuristic sync", tests);
      ("synchronisation heuristic ", tests_lwt);
    ]
  |> Lwt_main.run
