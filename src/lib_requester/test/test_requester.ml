(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Requester
    Invocation:   dune exec src/lib_requester/test/main.exe \
                  -- --file test_requester.ml
    Subject:      Basic behaviors of the API for generic resource
                  fetching/requesting service. Instantiating the [Requester]
                  functor with simple mocks.

                  [Memory_table] and [Disk_table] are hash tables from string
                  to int.
                  [Probe] either accepts or reject notified values based on
                  a boolean validation clue [Probe.param], regardless of
                  the key.
                  [Request] simply logs the requests made to [Request.send],
                  and considers only a unique, statically defined, active peer.
*)

open Testable
open Assert
open Lwt_assert
open Tztestable
include Shared

let precheck_pass = true

let precheck_fail = false

(** Requester-specific Alcotest testable instantiations *)

let testable_test_value : Parameters.value Alcotest.testable = Alcotest.int

let testable_test_key : Parameters.key Alcotest.testable = Alcotest.string

(** Test helpers *)

(** Lwt helpers *)

let is_resolved p = match Lwt.state p with Return _ -> true | _ -> false

let is_pending p = match Lwt.state p with Sleep -> true | _ -> false

(** Start tests *)

(** Creates a requester with [Disk_table] (of size 16) as the store. *)
let test_full_requester () = ignore (init_full_requester ())

(** Creates a requester with [Disk_table] (of size 16) as the store.
    Injects the key-value ("foo", 1), the operation result is
    disregarded.  Then, asserts that the key "foo" is present in memory
    table or disk.
*)
let test_full_requester_create _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  let* _ = Test_Requester.inject requester "foo" 1 in
  let* r = Test_Requester.known requester "foo" in
  assert_true "injected value is known" r ;
  Lwt.return_unit

(** Creates a full requester with a Lwt_watcher [global_input]. Fetches
    the value for keys "foo" and "bar" whenever they are
    known. Notifies the requester that a given value has been received
    for these keys. Finally, checks that this Lwt_watcher receives all
    notified values.
*)
let test_full_requester_create_with_global_input _ () =
  let open Lwt_syntax in
  let (global_input : (Parameters.key * Parameters.value) Lwt_watcher.input) =
    Lwt_watcher.create_input ()
  in
  let stream, stopper = Lwt_watcher.create_stream global_input in
  let requester = init_full_requester ~global_input () in
  (* Fetch two values *)
  let f1 = Test_Requester.fetch requester "foo" precheck_pass in
  let f2 = Test_Requester.fetch requester "bar" precheck_pass in
  (* Notify the two values *)
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "foo" 1 in
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "bar" 2 in
  (* resolve promises *)
  let* _ = f1 in
  let* _ = f2 in
  let* stream_list = Lwt_stream.nget 2 stream in
  check
    (list (pair string int))
    "obtained stream"
    [("foo", 1); ("bar", 2)]
    stream_list ;
  Lwt_watcher.shutdown stopper ;
  Lwt.return_unit

(** Creates a requester. At first, no key "baz" is known. When reading
    it with {!Test_Requester.read}, it shall fail with error
    [Missing_data]. When reading with {!Test_Requester.read_opt}, it
    returns [None]. Then, the key-value ("baz", 1) is injected, and
    this key is now known and can be read by both functions.
*)
let test_read_known_read_opt _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  let* b = Test_Requester.known requester "baz" in
  let* () = lwt_assert_false "empty requester has no values" b in
  let* r = Test_Requester.read requester "baz" in
  check
    (tzresults testable_test_value)
    "missing data"
    (Error [Test_Requester.Missing_data "baz"; Exn Not_found])
    r ;
  let* ro = Test_Requester.read_opt requester "baz" in
  check (option testable_test_value) "should be none" None ro ;
  let* _ = Test_Requester.inject requester "baz" 1 in
  let* b = Test_Requester.known requester "baz" in
  let* () = lwt_assert_true "baz is now known" b in
  let* r = Test_Requester.read requester "baz" in
  check (tzresults testable_test_value) "baz can be read" (Ok 1) r ;
  let* ro = Test_Requester.read_opt requester "baz" in
  check (option testable_test_value) "read_opt baz is (Some 1)" (Some 1) ro ;
  Lwt.return_unit

(** Creates a requester. At first, no key "boo" is known to the
    requester. It adds the key-value ("boo", 15) to the disk table and
    it is asserted that it is known by the latter. Hence, the requester
    now knows this key.
*)
let test_full_requester_disk_found_value _ () =
  let open Lwt_syntax in
  let requester, store = init_full_requester_disk () in
  let* b = Test_Requester.known requester "boo" in
  let* () = lwt_assert_false "empty requester has no values" b in
  (* add initial value 'boo' to disk requester *)
  Test_disk_table_hash.add store "boo" 15 ;
  let* b = Test_disk_table_hash.known store "boo" in
  let* () = lwt_assert_true "disk now knows value" b in
  (* now, fetching the value from disk requesters it in memory *)
  let* b = Test_Requester.known requester "boo" in
  lwt_assert_true "requester now knows value" b

(** Creates a requester. Perform key fetching with timeout of [0] at
    first, then with [0.1] picoseconds. Both tests are supposed to
    timeout (as the requester is empty).
*)
let test_full_requester_fetch_timeout _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  let do_timeout t v =
    let* res = Test_Requester.fetch ~timeout:t requester v precheck_pass in
    check
      (tzresults testable_test_value)
      "should timeout"
      (Error [Test_Requester.Timeout v])
      res ;
    Lwt.return_unit
  in
  let* () = do_timeout (Ptime.Span.of_int_s 0) "foo" in
  do_timeout
    (WithExceptions.Option.to_exn ~none:Not_found (Ptime.Span.of_float_s 0.1))
    "foo"

(** Creates a requester. Clears registered requests, then asserts that
    [!Test_request.registered_requests] is empty. Fetches the key "baz".
    At this point, it is expected that the number of registered requests
    is 5, and that "baz" is part of them.
*)
let test_full_fetch_issues_request _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  Test_request.clear_registered_requests () ;
  Alcotest.(
    check
      (list (tuple3 unit p2p_peer_id (list testable_test_key)))
      "should have no requests"
      []
      !Test_request.registered_requests) ;
  let f1 =
    Test_Requester.fetch
      ~timeout:
        (WithExceptions.Option.to_exn
           ~none:Not_found
           (Ptime.Span.of_float_s 0.1))
      requester
      "baz"
      precheck_pass
  in
  let* _ = f1 in
  (* expects 5 requests *)
  check
    int
    "expects 5 requests"
    5
    (List.length !Test_request.registered_requests) ;
  Alcotest.(
    check
      (tuple3 unit p2p_peer_id (list testable_test_key))
      "should have sent a request"
      ((), P2p_peer.Id.zero, ["baz"])
      (WithExceptions.Option.get ~loc:__LOC__
      @@ List.hd !Test_request.registered_requests)) ;
  Lwt.cancel f1 ;
  Lwt.return_unit

(** Creates a requester. Injects ("foo", 1), key "foo" is known.
    Removes this data from the memory table. This key is now unknown.
*)
let test_clear_or_cancel_removes _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  let* r =
    let* _ = Test_Requester.inject requester "foo" 1 in
    Test_Requester.known requester "foo"
  in
  assert_true "injected value is known" r ;
  Test_Requester.clear_or_cancel requester "foo" ;
  let* r = Test_Requester.known requester "foo" in
  lwt_assert_false "injected value is cleared" r

(** Creates a requester. Key "foo" is unknown yet. It is fetched,
    thereby pending. It is cancelled, thereby no longer pending. As of
    now, "foo" still remains unknown. The fetch operation itself
    indicates that is has been cancelled.
*)
let test_clear_or_cancel_cancels _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  (* request "foo" *)
  let* b = Test_Requester.known requester "foo" in
  let* () = lwt_assert_false "injected value is not known" b in
  let f1 = Test_Requester.fetch requester "foo" precheck_pass in
  assert_true "value is now pending" (Test_Requester.pending requester "foo") ;
  Test_Requester.clear_or_cancel requester "foo" ;
  assert_false
    "value is no longer pending after cancellation"
    (Test_Requester.pending requester "foo") ;
  let* r = Test_Requester.known requester "foo" in
  assert_false "injected value is cleared" r ;
  let* res = f1 in
  check
    (tzresults testable_test_value)
    "fetch returns cancellation"
    (Error [Test_Requester.Canceled "foo"])
    res ;
  Lwt.return_unit

(** Creates a requester. Key "foo" is unknown yet. It is fetched two times,
    thereby pending. It is cancelled one time, thereby still pending. After
    the second cancelation it is no longer pending. As of now, "foo" still
    remains unknown. The fetch operation itself  indicates that is has been
    cancelled.
*)
let test_clear_or_cancel_decrements _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  (* request "foo" *)
  let* b = Test_Requester.known requester "foo" in
  let* () = lwt_assert_false "injected value is not known" b in
  let f1 = Test_Requester.fetch requester "foo" precheck_pass in
  assert_true "value is now pending" (Test_Requester.pending requester "foo") ;
  let _f2 = Test_Requester.fetch requester "foo" precheck_pass in
  assert_true "value is now pending" (Test_Requester.pending requester "foo") ;
  Test_Requester.clear_or_cancel requester "foo" ;
  assert_true
    "value should still be pending after cancellation"
    (Test_Requester.pending requester "foo") ;
  Test_Requester.clear_or_cancel requester "foo" ;
  assert_false
    "value is no longer pending after cancellation"
    (Test_Requester.pending requester "foo") ;
  let* r = Test_Requester.known requester "foo" in
  assert_false "injected value is cleared" r ;
  let* res = f1 in
  check
    (tzresults testable_test_value)
    "fetch returns cancellation"
    (Error [Test_Requester.Canceled "foo"])
    res ;
  Lwt.return_unit

(** Test pending *)

(** Creates a requester. Initially, no key "foo" is pending. After
    calling the fetch operation, the key becomes pending. After
    cancelling, the key is no longer pending.
*)
let test_pending_cancelled _ () =
  let requester = init_full_requester () in
  assert_false
    "value is not pending initially"
    (Test_Requester.pending requester "foo") ;
  ignore (Test_Requester.fetch requester "foo" precheck_pass) ;
  assert_true
    "value is pending after fetch"
    (Test_Requester.pending requester "foo") ;
  Test_Requester.clear_or_cancel requester "foo" ;
  lwt_assert_false
    "value is no longer pending after cancellation"
    (Test_Requester.pending requester "foo")

(** Checks that values are not pending after notification *)
let test_pending_notified _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  assert_false
    "value is not pending initially"
    (Test_Requester.pending requester "foo") ;
  ignore (Test_Requester.fetch requester "foo" precheck_pass) ;
  assert_true
    "value is pending after fetch"
    (Test_Requester.pending requester "foo") ;
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "foo" 1 in
  lwt_assert_false
    "value is no longer pending after notification"
    (Test_Requester.pending requester "foo")

(** Check that values are not pending after timeout *)
let test_pending_timeout _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  assert_false
    "value is not pending initially"
    (Test_Requester.pending requester "foo") ;
  let f1 =
    Test_Requester.fetch
      ~timeout:
        (WithExceptions.Option.to_exn
           ~none:Not_found
           (Ptime.Span.of_float_s 0.001))
      requester
      "foo"
      precheck_pass
  in
  assert_true
    "value is pending after fetch"
    (Test_Requester.pending requester "foo") ;
  let* res = f1 in
  assert (res = Error [Test_Requester.Timeout "foo"]) ;
  lwt_assert_false
    "value is no longer pending after timeout"
    (Test_Requester.pending requester "foo")

(** Test watch *)

(** Creates a requester. Adds a watcher to the requester. Fetch keys
   "foo", "bar". Notify both values to the requester. Finally, ensures
   that both are watched.
*)
let test_full_requester_test_simple_watch _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  let stream, stopper = Test_Requester.watch requester in
  (* Fetch two values *)
  let f1 = Test_Requester.fetch requester "foo" precheck_pass in
  let f2 = Test_Requester.fetch requester "bar" precheck_pass in
  (* Notify the two values *)
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "foo" 1 in
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "bar" 2 in
  (* resolve promises *)
  let* _ = f1 in
  let* _ = f2 in
  let* stream_list = Lwt_stream.nget 2 stream in
  check
    (list (pair string int))
    "obtained stream"
    [("foo", 1); ("bar", 2)]
    stream_list ;
  Lwt_watcher.shutdown stopper ;
  Lwt.return_unit

(** Add a watcher, notify a value that is not requested. The
    stream that is watched will remain empty in the end.
*)
let test_full_requester_test_notify_non_fetched_watch _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  let stream, stopper = Test_Requester.watch requester in
  (* Notify the a value that not been requested, should be ignored and
     hence not visible to the watcher. *)
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "foo" 1 in
  Lwt_watcher.shutdown stopper ;
  let* b = Lwt_stream.is_empty stream in
  lwt_assert_true "obtained stream should be empty" b

(** Add two watchers, verify that both receive notified values.
    Stop one watcher, verify that the remaining receives notified values.
*)
let test_full_requester_test_double_watcher _ () =
  let open Lwt_syntax in
  let requester = init_full_requester () in
  let stream1, stopper1 = Test_Requester.watch requester in
  let stream2, stopper2 = Test_Requester.watch requester in
  (* Fetch a values *)
  let f1 = Test_Requester.fetch requester "foo" precheck_pass in
  (* Notify the value *)
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "foo" 1 in
  (* resolve promises *)
  let* _ = f1 in
  (* check first stream *)
  let* stream_list1 = Lwt_stream.nget 1 stream1 in
  (check (list (pair string int))) "obtained stream1" [("foo", 1)] stream_list1 ;
  (* check second stream *)
  let* stream_list2 = Lwt_stream.nget 1 stream2 in
  (check (list (pair string int))) "obtained stream2" [("foo", 1)] stream_list2 ;
  (* shutdown first stream *)
  Lwt_watcher.shutdown stopper1 ;
  (* Fetch a values *)
  let f2 = Test_Requester.fetch requester "bar" precheck_pass in
  (* Notify the value *)
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "bar" 2 in
  (* resolve promises *)
  let* _ = f2 in
  (* verify that the first stream is empty *)
  assert_true "stream1 is empty" (Lwt_stream.is_closed stream1) ;
  (* check second stream received the value *)
  let* stream_list2 = Lwt_stream.nget 1 stream2 in
  (check (list (pair string int)))
    "obtained second value in stream2 "
    [("bar", 2)]
    stream_list2 ;
  Lwt_watcher.shutdown stopper2 ;
  Lwt.return_unit

(** Test inject *)

(** Injects a value already present in memory: false should be
    returned.
*)
let test_full_requester_test_inject_memory _ () =
  let open Lwt_syntax in
  let req = init_full_requester () in
  let* b = Test_Requester.inject req "foo" 1 in
  let* () = lwt_assert_true "Inject is true  first time" b in
  let* b = Test_Requester.inject req "foo" 1 in
  let* () = lwt_assert_false "Inject is false second time" b in
  let* b = Test_Requester.inject req "foo" 2 in
  lwt_assert_false "Inject is false third time with new value" b

(** Injects a value present on disk: false should be returned. *)
let test_full_requester_test_inject_disk _ () =
  let open Lwt_syntax in
  let req, store = init_full_requester_disk () in
  Test_disk_table_hash.add store "foo" 1 ;
  let* b = Test_Requester.inject req "foo" 1 in
  lwt_assert_false "Inject is false when present on disk" b

(** Injects a value already requested: false should be returned. *)
let test_full_requester_test_inject_requested _ () =
  let open Lwt_syntax in
  let req = init_full_requester () in
  (* Fetch a value *)
  ignore (Test_Requester.fetch req "foo" precheck_pass) ;
  let* b = Test_Requester.inject req "foo" 1 in
  let* () = lwt_assert_false "Inject is false when already requested" b in
  Test_Requester.clear_or_cancel req "foo" ;
  Lwt.return_unit

(** Injects a value not yet requested: true is returned *)
let test_full_requester_test_inject _ () =
  let open Lwt_syntax in
  let req = init_full_requester () in
  let* b = Test_Requester.inject req "foo" 1 in
  let* () =
    lwt_assert_true
      "Inject is true as value not in disk/mem/already requested"
      b
  in
  Lwt.return_unit

(** Test notify *)

(** Notifies a value with an invalid value. The memory table should not
    be updated and the promises not resolved.
*)
let test_full_requester_test_notify_invalid _ () =
  let open Lwt_syntax in
  let req = init_full_requester () in
  let* b = Test_Requester.known req "foo" in
  let* () = lwt_assert_false "fetched value is not known" b in
  (* Fetch invalid value  *)
  let f1 = Test_Requester.fetch req "foo" precheck_fail in
  (* Notify value *)
  let* () = Test_Requester.notify req P2p_peer.Id.zero "foo" 1 in
  let* b = Test_Requester.known req "foo" in
  let* () = lwt_assert_false "fetched value is still not known" b in
  lwt_assert_true "promise is still pending" (is_pending f1)

(** Notifies a value with a valid value. The memory table should be
    updated, the promise resolved.
*)
let test_full_requester_test_notify_valid _ () =
  let open Lwt_syntax in
  let req = init_full_requester () in
  (* Fetch valid value  *)
  let f1 = Test_Requester.fetch req "foo" precheck_pass in
  (* Notify value *)
  let* () = Test_Requester.notify req P2p_peer.Id.zero "foo" 1 in
  let* b = Test_Requester.known req "foo" in
  let* () = lwt_assert_true "fetched value is now known" b in
  let* () = Lwt.pause () (* Ensure that [f1] is scheduled *) in
  lwt_assert_true "promise is resolved" (is_resolved f1)

(** Notifies a value that has not been fetched. The notification is
    simply ignored, and value remains unknown.
*)
let test_full_requester_test_notify_unfetched _ () =
  let open Lwt_syntax in
  let req = init_full_requester () in
  (* Notify value that has not been fetched *)
  let* () = Test_Requester.notify req P2p_peer.Id.zero "foo" 1 in
  let* b = Test_Requester.known req "foo" in
  lwt_assert_false "fetched value is not known" b

(** Notifies a value that is already on disk. The notification should
    be ignored (not sure how to test this, but this code runs through
    that code path).  *)
let test_full_requester_test_notify_disk_duplicate _ () =
  let req, store = init_full_requester_disk () in
  (* Put value on disk *)
  Test_disk_table_hash.add store "foo" 1 ;
  (* Fetch valid value  *)
  ignore (Test_Requester.fetch req "foo" precheck_pass) ;
  (* Notify the value *)
  Test_Requester.notify req P2p_peer.Id.zero "foo" 1

(** Notifies a value that is already in memory. The notification should
    be ignored (not sure how to test this, but this code runs through
    that code path).
*)
let test_full_requester_test_notify_memory_duplicate _ () =
  let open Lwt_syntax in
  let req = init_full_requester () in
  (* Put value in memory *)
  let* _ = Test_Requester.inject req "foo" 1 in
  (* Fetch valid value  *)
  ignore (Test_Requester.fetch req "foo" precheck_pass) ;
  (* Notify the value *)
  Test_Requester.notify req P2p_peer.Id.zero "foo" 1

(** Test pending requests *)

(** Notifies a value that has not been fetched. The notification should
    be ignored.
*)
let test_full_requester_test_pending_requests _ () =
  let open Lwt_syntax in
  let req = init_full_requester () in
  let check_pending_count msg count =
    (check int) msg count (Test_Requester.pending_requests req)
  in
  let with_request key k =
    Lwt.join
      [
        (let* _ = Test_Requester.fetch req key precheck_pass in
         Lwt.return_unit);
        (* Ensure that the request is registered before [k] is scheduled. *)
        (let* v = Lwt.pause () in
         k v);
      ]
  in
  (* Variant of [with_request] for requests that are never satisfied. When [k]
     returns, the request is left pending. *)
  let with_unmet_request key k =
    Lwt.choose
      [
        (let+ _ = Test_Requester.fetch req key precheck_pass in
         Alcotest.fail "Request should not have been satisfied");
        (let* v = Lwt.pause () in
         k v);
      ]
  in
  (* Fetch value  *)
  check_pending_count "0 pending requests" 0 ;
  let foo_cancelled : unit Lwt.t =
    with_request "foo" @@ fun () ->
    check_pending_count "1 pending requests" 1 ;
    with_unmet_request "bar" @@ fun () ->
    check_pending_count "2 pending requests" 2 ;
    with_unmet_request "bar" @@ fun () ->
    check_pending_count "still 2 pending requests" 2 ;
    Lwt.return (Test_Requester.clear_or_cancel req "foo")
  in
  let+ () = foo_cancelled in
  check_pending_count "back to 1 pending requests" 1

(** Test memory_table_length *)

(** Injects some values and checks the length of the memory table. *)
let test_full_requester_test_memory_table_length _ () =
  let open Lwt_syntax in
  let req = init_full_requester () in
  (check int) "0 cached values" 0 (Test_Requester.memory_table_length req) ;
  let* _ = Test_Requester.inject req "foo" 1 in
  (check int) "1 cached values" 1 (Test_Requester.memory_table_length req) ;
  let* _ = Test_Requester.inject req "bar" 2 in
  (check int) "2 cached values" 2 (Test_Requester.memory_table_length req) ;
  let* _ = Test_Requester.inject req "bar" 2 in
  (check int) "still 2 cached values" 2 (Test_Requester.memory_table_length req) ;
  let* _ = Test_Requester.inject req "baz" 3 in
  (check int) "now 3 cached values" 3 (Test_Requester.memory_table_length req) ;
  Lwt.return_unit

(** Test shutdown *)

let test_full_requester_shutdown _ () =
  let req = init_full_requester () in
  Test_Requester.shutdown req

let () =
  Alcotest_lwt.run
    ~__FILE__
    "tezos-requester"
    [
      ( "all",
        [
          Alcotest_lwt.test_case_sync
            "test create: simple"
            `Quick
            test_full_requester;
          Alcotest_lwt.test_case
            "test create: test known"
            `Quick
            test_full_requester_create;
          Alcotest_lwt.test_case
            "test create: with global_input"
            `Quick
            test_full_requester_create_with_global_input;
          Alcotest_lwt.test_case
            "test read: (and variations)"
            `Quick
            test_read_known_read_opt;
          Alcotest_lwt.test_case
            "test known: found values in disk are cached"
            `Quick
            test_full_requester_disk_found_value;
          Alcotest_lwt.test_case
            "test fetch: full requester timeout"
            `Quick
            test_full_requester_fetch_timeout;
          Alcotest_lwt.test_case
            "test fetch: full requester issues request"
            `Quick
            test_full_fetch_issues_request;
          Alcotest_lwt.test_case
            "test clear_or_cancel: removes"
            `Quick
            test_clear_or_cancel_removes;
          Alcotest_lwt.test_case
            "test clear_or_cancel: decrements pending"
            `Quick
            test_clear_or_cancel_decrements;
          Alcotest_lwt.test_case
            "test clear_or_cancel: cancels"
            `Quick
            test_clear_or_cancel_cancels;
          Alcotest_lwt.test_case
            "test pending cancelled"
            `Quick
            test_pending_cancelled;
          Alcotest_lwt.test_case
            "test pending notified"
            `Quick
            test_pending_notified;
          Alcotest_lwt.test_case
            "test pending timeout"
            `Quick
            test_pending_timeout;
          Alcotest_lwt.test_case
            "test watch: simple"
            `Quick
            test_full_requester_test_simple_watch;
          Alcotest_lwt.test_case
            "test watch: non fetched"
            `Quick
            test_full_requester_test_notify_non_fetched_watch;
          Alcotest_lwt.test_case
            "test watch: double watchers"
            `Quick
            test_full_requester_test_double_watcher;
          Alcotest_lwt.test_case
            "test inject: already in memory"
            `Quick
            test_full_requester_test_inject_memory;
          Alcotest_lwt.test_case
            "test inject: already in disk"
            `Quick
            test_full_requester_test_inject_disk;
          Alcotest_lwt.test_case
            "test inject: already in requested"
            `Quick
            test_full_requester_test_inject_requested;
          Alcotest_lwt.test_case
            "test inject: otherwise"
            `Quick
            test_full_requester_test_inject;
          Alcotest_lwt.test_case
            "test notify: invalid"
            `Quick
            test_full_requester_test_notify_invalid;
          Alcotest_lwt.test_case
            "test notify: valid"
            `Quick
            test_full_requester_test_notify_valid;
          Alcotest_lwt.test_case
            "test notify: unfetched"
            `Quick
            test_full_requester_test_notify_unfetched;
          Alcotest_lwt.test_case
            "test notify: memory duplicate"
            `Quick
            test_full_requester_test_notify_memory_duplicate;
          Alcotest_lwt.test_case
            "test notify: disk duplicate"
            `Quick
            test_full_requester_test_notify_disk_duplicate;
          Alcotest_lwt.test_case
            "test pending_requests"
            `Quick
            test_full_requester_test_pending_requests;
          Alcotest_lwt.test_case
            "test memory_table_length"
            `Quick
            test_full_requester_test_memory_table_length;
          Alcotest_lwt.test_case
            "test shutdown"
            `Quick
            test_full_requester_shutdown;
        ] );
    ]
  |> Lwt_main.run
