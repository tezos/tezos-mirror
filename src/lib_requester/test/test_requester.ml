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

include Shared

let common_tags = ["requester"; "quick"]

let precheck_pass = true

let precheck_fail = false

(** Requester-specific testable instantiations *)

let value_typ : Parameters.value Check.typ = Check.int

let key_typ : Parameters.key Check.typ = Check.string

(** Test helpers *)

(** Lwt helpers *)

let is_resolved p = match Lwt.state p with Return _ -> true | _ -> false

let is_pending p = match Lwt.state p with Sleep -> true | _ -> false

(** Start tests *)
let tags = "start" :: common_tags

(** Creates a requester with [Disk_table] (of size 16) as the store. *)
let test_full_requester () =
  Test.register ~__FILE__ ~title:"test create: simple" ~tags @@ fun () ->
  let (_ : Test_Requester.t) = init_full_requester () in
  unit

(** Creates a requester with [Disk_table] (of size 16) as the store.
    Injects the key-value ("foo", 1), the operation result is
    disregarded.  Then, asserts that the key "foo" is present in memory
    table or disk.
*)
let test_full_requester_create () =
  Test.register ~__FILE__ ~title:"test create: test known" ~tags @@ fun () ->
  let requester = init_full_requester () in
  let* _ = Test_Requester.inject requester "foo" 1 in
  let* r = Test_Requester.known requester "foo" in
  if not r then
    Test.fail ~__LOC__ "injected value is not known but it should be." ;
  unit

(** Creates a full requester with a Lwt_watcher [global_input]. Fetches
    the value for keys "foo" and "bar" whenever they are
    known. Notifies the requester that a given value has been received
    for these keys. Finally, checks that this Lwt_watcher receives all
    notified values.
*)
let test_full_requester_create_with_global_input () =
  Test.register ~__FILE__ ~title:"test create: with global_input" ~tags
  @@ fun () ->
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
  Check.(
    (stream_list = [("foo", 1); ("bar", 2)])
      (list (tuple2 string int))
      ~__LOC__
      ~error_msg:"obtained stream %L, expected %R") ;
  Lwt_watcher.shutdown stopper ;
  unit

let tags = common_tags

(** Creates a requester. At first, no key "baz" is known. When reading
    it with {!Test_Requester.read}, it shall fail with error
    [Missing_data]. When reading with {!Test_Requester.read_opt}, it
    returns [None]. Then, the key-value ("baz", 1) is injected, and
    this key is now known and can be read by both functions.
*)
let test_read_known_read_opt () =
  Test.register ~__FILE__ ~title:"test read: (and variations)" ~tags
  @@ fun () ->
  let requester = init_full_requester () in
  let* b = Test_Requester.known requester "baz" in
  if b then Test.fail "empty requester has no values" ;
  let* r = Test_Requester.read requester "baz" in
  Check.(
    (r = Error [Test_Requester.Missing_data "baz"; Exn Not_found])
      (Tzcheck.tzresult value_typ)
      ~__LOC__
      ~error_msg:"missing data, obtained %L, expected %R") ;
  let* ro = Test_Requester.read_opt requester "baz" in
  Check.(
    (ro = None)
      (option value_typ)
      ~__LOC__
      ~error_msg:"oblained %L, should be none") ;
  let* _ = Test_Requester.inject requester "baz" 1 in
  let* b = Test_Requester.known requester "baz" in
  if not b then Test.fail "baz is now known" ;
  let* r = Test_Requester.read requester "baz" in
  Check.(
    (r = Ok 1)
      (Tzcheck.tzresult value_typ)
      ~__LOC__
      ~error_msg:"baz can be read, but result is %L") ;
  let* ro = Test_Requester.read_opt requester "baz" in
  Check.(
    (ro = Some 1)
      (option value_typ)
      ~__LOC__
      ~error_msg:"read_opt baz should be (Some 1) but is %L") ;
  unit

(** Creates a requester. At first, no key "boo" is known to the
    requester. It adds the key-value ("boo", 15) to the disk table and
    it is asserted that it is known by the latter. Hence, the requester
    now knows this key.
*)
let test_full_requester_disk_found_value () =
  Test.register
    ~__FILE__
    ~title:"test known: found values in disk are cached"
    ~tags
  @@ fun () ->
  let requester, store = init_full_requester_disk () in
  let* b = Test_Requester.known requester "boo" in
  if b then Test.fail "empty requester has no values" ;
  (* add initial value 'boo' to disk requester *)
  Test_disk_table_hash.add store "boo" 15 ;
  let* b = Test_disk_table_hash.known store "boo" in
  if not b then Test.fail "disk now knows value" ;
  (* now, fetching the value from disk requesters it in memory *)
  let* b = Test_Requester.known requester "boo" in
  if not b then Test.fail "requester now knows value" ;
  unit

(** Creates a requester. Perform key fetching with timeout of [0] at
    first, then with [0.1] picoseconds. Both tests are supposed to
    timeout (as the requester is empty).
*)
let test_full_requester_fetch_timeout () =
  Test.register ~__FILE__ ~title:"test fetch: full requester timeout" ~tags
  @@ fun () ->
  let requester = init_full_requester () in
  let do_timeout t v =
    let* res = Test_Requester.fetch ~timeout:t requester v precheck_pass in
    Check.(
      (res = Error [Test_Requester.Timeout v])
        (Tzcheck.tzresult value_typ)
        ~__LOC__
        ~error_msg:"should timeout, but returned %L") ;
    unit
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
let test_full_fetch_issues_request () =
  Test.register
    ~__FILE__
    ~title:"test fetch: full requester issues request"
    ~tags:
      ((* FIXME https://gitlab.com/tezos/tezos/-/issues/6635
          The following test is flaky and is thus deactivated. When run in opam
          tests in particular, it fails often. It looks like it expects a
          number that depends on how long a timeout actually takes in practice,
          which is inherently flaky. *)
       Tezos_test_helpers.Tag.flaky :: tags)
  @@ fun () ->
  let requester = init_full_requester () in
  Test_request.reinitialize () ;
  Check.(
    (!Test_request.registered_requests = [])
      (list (tuple3 unit Tzcheck.p2p_peer_id (list key_typ)))
      ~__LOC__
      ~error_msg:"should have no requests, but has %L") ;
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
  Check.(
    (List.length !Test_request.registered_requests = 5)
      int
      ~__LOC__
      ~error_msg:"expects 5 requests, but has %L requests") ;
  Check.(
    (WithExceptions.Option.get ~loc:__LOC__
     @@ List.hd !Test_request.registered_requests
    = ((), P2p_peer.Id.zero, ["baz"]))
      (tuple3 unit Tzcheck.p2p_peer_id (list key_typ))
      ~__LOC__
      ~error_msg:"should have sent a request, but returned %L") ;
  Lwt.cancel f1 ;
  unit

(** Creates a requester. Clears registered requests, then asserts that
    [!Test_request.registered_requests] is empty. Add 4 peers to active.
    Fetches the key "baz" 3 times with 3 different peers. Check that the 3
    first registered request are on different peers and that the unused peer
    has not been requested.
*)
let test_full_fetch_request_unrequested_peers_first () =
  Test.register
    ~__FILE__
    ~title:"test inject: request unrequested peers first"
    ~tags
  @@ fun () ->
  let make_peer i =
    match
      P2p_peer.Id.(
        of_string (Stdlib.String.make size (Char.chr (Char.code '0' + i))))
    with
    | Ok p -> p
    | _ -> assert false
  in
  let requestable_peers =
    P2p_peer.Set.of_list (WithExceptions.List.init ~loc:__LOC__ 3 make_peer)
  in
  let unused_peer = make_peer 4 in
  let requester = init_full_requester () in
  Test_request.reinitialize () ;
  (* To ensure that there is not a second request sent before all fetches has
     been done, the initial delay is raised. *)
  Test_request.initial_delay_val := Time.System.Span.of_seconds_exn 0.1 ;
  Check.(
    (!Test_request.registered_requests = [])
      (list (tuple3 unit Tzcheck.p2p_peer_id (list key_typ)))
      ~__LOC__
      ~error_msg:"should have no request, but has %L") ;
  Test_request.active_peers := P2p_peer.Set.add unused_peer requestable_peers ;
  (* Fetch one time for each peer that should be requested for this data. *)
  let fetch_promises =
    P2p_peer.Set.fold
      (fun peer promises ->
        Test_Requester.fetch
          ~peer
          ~timeout:
            (WithExceptions.Option.to_exn
               ~none:Not_found
               (Ptime.Span.of_float_s 1.))
          requester
          "baz"
          precheck_pass
        :: promises)
      requestable_peers
      []
  in
  (* Wait that all fetches time out to ensure that all requests have been done
     before checking them. *)
  let* fetches_tzresults = Lwt_result_syntax.all fetch_promises in
  (match fetches_tzresults with
  | Error errs ->
      Format.eprintf "size: %d@." (List.length errs) ;
      List.iter
        (function
          | [Test_Requester.Timeout "baz"] -> ()
          | _ -> Test.fail ~__LOC__ "all fetch requests should have timed out")
        errs
  | _ -> Test.fail ~__LOC__ "all fetch requests should have timed out") ;
  if
    List.exists
      (fun (_, peer, _) -> P2p_peer.Id.equal peer unused_peer)
      !Test_request.registered_requests
  then Test.fail ~__LOC__ "unused_peer should not have been used" ;
  (* Look over the list of requests to get the set of peers requested and
     determine if all requestable peers have been requested once before any
     peer has been requested a second time. *)
  let seen_peers, ok =
    List.fold_right
      (fun request (seen_peers, ok) ->
        match (ok, request) with
        | true, ((), peer, ["baz"]) ->
            if P2p_peer.Set.equal seen_peers requestable_peers then
              (* If all requestable peers have been already requested,
                 these peers can be requested multiple times. *)
              (P2p_peer.Set.add peer seen_peers, ok)
            else if P2p_peer.Set.mem peer seen_peers then
              (* If not all requestable peers have been requested, no peer
                 should be requested a second time. *)
              (seen_peers, false)
            else
              (* A peer has been requested for the first time, it is added to
                 the seen peers. *)
              (P2p_peer.Set.add peer seen_peers, ok)
        | _ -> (seen_peers, false))
      !Test_request.registered_requests
      (P2p_peer.Set.empty, true)
  in
  Check.(
    (( !Test_request.registered_requests,
       List.of_seq (P2p_peer.Set.to_seq seen_peers),
       ok )
    = ( !Test_request.registered_requests,
        List.of_seq (P2p_peer.Set.to_seq requestable_peers),
        true ))
      (tuple3
         ((* The chronological list of requests.
             It is useful to diagnostic any result. *)
          list
            (tuple3 unit Tzcheck.p2p_peer_id (list key_typ)))
         ((* The set of requestable peers is expected and compared to the set
             of requested peers.
             It permits to determine that all requestable peers and only these
             peers have been requested. *)
          list
            Tzcheck.p2p_peer_id)
         (* True if each requestable peer has been requested before any peer
            has been requested a second time. It is expected to be true. *)
         bool)
      ~__LOC__
      ~error_msg:
        "all requestable_peers should be requested once before rerequesting a \
         peer, but it is not the case in returned values %L") ;
  (* Clean up. *)
  Test_request.reinitialize () ;
  unit

(** Creates a requester. Injects ("foo", 1), key "foo" is known.
    Removes this data from the memory table. This key is now unknown.
*)
let test_clear_or_cancel_removes () =
  Test.register ~__FILE__ ~title:"test clear_or_cancel: removes" ~tags
  @@ fun () ->
  let requester = init_full_requester () in
  let* r =
    let* _ = Test_Requester.inject requester "foo" 1 in
    Test_Requester.known requester "foo"
  in
  if not r then Test.fail "injected value is known" ;
  Test_Requester.clear_or_cancel requester "foo" ;
  let* r = Test_Requester.known requester "foo" in
  if r then Test.fail "injected value is cleared" ;
  unit

(** Creates a requester. Key "foo" is unknown yet. It is fetched,
    thereby pending. It is cancelled, thereby no longer pending. As of
    now, "foo" still remains unknown. The fetch operation itself
    indicates that is has been cancelled.
*)
let test_clear_or_cancel_cancels () =
  Test.register ~__FILE__ ~title:"test clear_or_cancel: cancels" ~tags
  @@ fun () ->
  let requester = init_full_requester () in
  (* request "foo" *)
  let* b = Test_Requester.known requester "foo" in
  if b then Test.fail "injected value is not known" ;
  let f1 = Test_Requester.fetch requester "foo" precheck_pass in
  if not (Test_Requester.pending requester "foo") then
    Test.fail "value is now pending" ;
  Test_Requester.clear_or_cancel requester "foo" ;
  if Test_Requester.pending requester "foo" then
    Test.fail "value is no longer pending after cancellation" ;
  let* r = Test_Requester.known requester "foo" in
  if r then Test.fail "injected value is cleared" ;
  let* res = f1 in
  Check.(
    (res = Error [Test_Requester.Canceled "foo"])
      (Tzcheck.tzresult value_typ)
      ~__LOC__
      ~error_msg:"fetch should return cancellation, but returned %L") ;
  unit

(** Creates a requester. Key "foo" is unknown yet. It is fetched two times,
    thereby pending. It is cancelled one time, thereby still pending. After
    the second cancelation it is no longer pending. As of now, "foo" still
    remains unknown. The fetch operation itself  indicates that is has been
    cancelled.
*)
let test_clear_or_cancel_decrements () =
  Test.register
    ~__FILE__
    ~title:"test clear_or_cancel: decrements pending"
    ~tags
  @@ fun () ->
  let requester = init_full_requester () in
  (* request "foo" *)
  let* b = Test_Requester.known requester "foo" in
  if b then Test.fail "injected value is not known" ;
  let f1 = Test_Requester.fetch requester "foo" precheck_pass in
  if not (Test_Requester.pending requester "foo") then
    Test.fail "value is now pending" ;
  let _f2 = Test_Requester.fetch requester "foo" precheck_pass in
  if not (Test_Requester.pending requester "foo") then
    Test.fail "value is now pending" ;
  Test_Requester.clear_or_cancel requester "foo" ;
  if not (Test_Requester.pending requester "foo") then
    Test.fail "value should still be pending after cancellation" ;
  Test_Requester.clear_or_cancel requester "foo" ;
  if Test_Requester.pending requester "foo" then
    Test.fail "value is no longer pending after cancellation" ;
  let* r = Test_Requester.known requester "foo" in
  if r then Test.fail "injected value is cleared" ;
  let* res = f1 in
  Check.(
    (res = Error [Test_Requester.Canceled "foo"])
      (Tzcheck.tzresult value_typ)
      ~__LOC__
      ~error_msg:"fetch should return cancellation, but returned %L") ;
  unit

(** Test pending *)
let tags = "pending" :: common_tags

(** Creates a requester. Initially, no key "foo" is pending. After
    calling the fetch operation, the key becomes pending. After
    cancelling, the key is no longer pending.
*)
let test_pending_cancelled () =
  Test.register ~__FILE__ ~title:"test pending cancelled" ~tags @@ fun () ->
  let requester = init_full_requester () in
  if Test_Requester.pending requester "foo" then
    Test.fail "value is not pending initially" ;
  ignore (Test_Requester.fetch requester "foo" precheck_pass) ;
  if not (Test_Requester.pending requester "foo") then
    Test.fail "value is pending after fetch" ;
  Test_Requester.clear_or_cancel requester "foo" ;
  if Test_Requester.pending requester "foo" then
    Test.fail "value is no longer pending after cancellation" ;
  unit

(** Checks that values are not pending after notification *)
let test_pending_notified () =
  Test.register ~__FILE__ ~title:"test watch: simple" ~tags @@ fun () ->
  let requester = init_full_requester () in
  if Test_Requester.pending requester "foo" then
    Test.fail "value is not pending initially" ;
  ignore (Test_Requester.fetch requester "foo" precheck_pass) ;
  if not (Test_Requester.pending requester "foo") then
    Test.fail "value is pending after fetch" ;
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "foo" 1 in
  if Test_Requester.pending requester "foo" then
    Test.fail "value is no longer pending after notification" ;
  unit

(** Check that values are not pending after timeout *)
let test_pending_timeout () =
  Test.register ~__FILE__ ~title:"test pending notified" ~tags @@ fun () ->
  let requester = init_full_requester () in
  if Test_Requester.pending requester "foo" then
    Test.fail "value is not pending initially" ;
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
  if not (Test_Requester.pending requester "foo") then
    Test.fail "value is pending after fetch" ;
  let* res = f1 in
  assert (res = Error [Test_Requester.Timeout "foo"]) ;
  if Test_Requester.pending requester "foo" then
    Test.fail "value is no longer pending after timeout" ;
  unit

(** Test watch *)
let tags = "watch" :: common_tags

(** Creates a requester. Adds a watcher to the requester. Fetch keys
   "foo", "bar". Notify both values to the requester. Finally, ensures
   that both are watched.
*)
let test_full_requester_test_simple_watch () =
  Test.register ~__FILE__ ~title:"test pending timeout" ~tags @@ fun () ->
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
  Check.(
    (stream_list = [("foo", 1); ("bar", 2)])
      (list (tuple2 string int))
      ~__LOC__
      ~error_msg:"obtained stream %L, expected %R") ;
  Lwt_watcher.shutdown stopper ;
  unit

(** Add a watcher, notify a value that is not requested. The
    stream that is watched will remain empty in the end.
*)
let test_full_requester_test_notify_non_fetched_watch () =
  Test.register ~__FILE__ ~title:"test watch: non fetched" ~tags @@ fun () ->
  let requester = init_full_requester () in
  let stream, stopper = Test_Requester.watch requester in
  (* Notify the a value that not been requested, should be ignored and
     hence not visible to the watcher. *)
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "foo" 1 in
  Lwt_watcher.shutdown stopper ;
  let* b = Lwt_stream.is_empty stream in
  if not b then Test.fail "obtained stream should be empty" ;
  unit

(** Add two watchers, verify that both receive notified values.
    Stop one watcher, verify that the remaining receives notified values.
*)
let test_full_requester_test_double_watcher () =
  Test.register ~__FILE__ ~title:"test watch: double watchers" ~tags
  @@ fun () ->
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
  Check.(
    (stream_list1 = [("foo", 1)])
      (list (tuple2 string int))
      ~__LOC__
      ~error_msg:"obtained stream1 %L, expected %R") ;
  (* check second stream *)
  let* stream_list2 = Lwt_stream.nget 1 stream2 in
  Check.(
    (stream_list2 = [("foo", 1)])
      (list (tuple2 string int))
      ~__LOC__
      ~error_msg:"obtained stream2 %L, expected %R") ;
  (* shutdown first stream *)
  Lwt_watcher.shutdown stopper1 ;
  (* Fetch a values *)
  let f2 = Test_Requester.fetch requester "bar" precheck_pass in
  (* Notify the value *)
  let* () = Test_Requester.notify requester P2p_peer.Id.zero "bar" 2 in
  (* resolve promises *)
  let* _ = f2 in
  (* verify that the first stream is empty *)
  if not (Lwt_stream.is_closed stream1) then Test.fail "stream1 is empty" ;
  (* check second stream received the value *)
  let* stream_list2 = Lwt_stream.nget 1 stream2 in
  Check.(
    (stream_list2 = [("bar", 2)])
      (list (tuple2 string int))
      ~__LOC__
      ~error_msg:"obtained second value in stream2 is %L, expected was %R") ;
  Lwt_watcher.shutdown stopper2 ;
  unit

(** Test inject *)

let tags = "inject" :: common_tags

(** Injects a value already present in memory: false should be
    returned.
*)
let test_full_requester_test_inject_memory () =
  Test.register ~__FILE__ ~title:"test inject: already in memory" ~tags
  @@ fun () ->
  let req = init_full_requester () in
  let* b = Test_Requester.inject req "foo" 1 in
  if not b then Test.fail "Inject is true  first time" ;
  let* b = Test_Requester.inject req "foo" 1 in
  if b then Test.fail "Inject is false second time" ;
  let* b = Test_Requester.inject req "foo" 2 in
  if b then Test.fail "Inject is false third time with new value" ;
  unit

(** Injects a value present on disk: false should be returned. *)
let test_full_requester_test_inject_disk () =
  Test.register ~__FILE__ ~title:"test inject: already in disk" ~tags
  @@ fun () ->
  let req, store = init_full_requester_disk () in
  Test_disk_table_hash.add store "foo" 1 ;
  let* b = Test_Requester.inject req "foo" 1 in
  if b then Test.fail "Inject is false when present on disk" ;
  unit

(** Injects a value already requested: false should be returned. *)
let test_full_requester_test_inject_requested () =
  Test.register ~__FILE__ ~title:"test inject: already in requested" ~tags
  @@ fun () ->
  let req = init_full_requester () in
  (* Fetch a value *)
  ignore (Test_Requester.fetch req "foo" precheck_pass) ;
  let* b = Test_Requester.inject req "foo" 1 in
  if b then Test.fail "Inject is false when already requested" ;
  Test_Requester.clear_or_cancel req "foo" ;
  unit

(** Injects a value not yet requested: true is returned *)
let test_full_requester_test_inject () =
  Test.register ~__FILE__ ~title:"test inject: otherwise" ~tags @@ fun () ->
  let req = init_full_requester () in
  let* b = Test_Requester.inject req "foo" 1 in
  if not b then
    Test.fail "Inject is true as value not in disk/mem/already requested" ;
  unit

(** Test notify *)
let tags = "notify" :: common_tags

(** Notifies a value with an invalid value. The memory table should not
    be updated and the promises not resolved.
*)
let test_full_requester_test_notify_invalid () =
  Test.register ~__FILE__ ~title:"test notify: invalid" ~tags @@ fun () ->
  let req = init_full_requester () in
  let* b = Test_Requester.known req "foo" in
  if b then Test.fail "fetched value is not known" ;
  (* Fetch invalid value  *)
  let f1 = Test_Requester.fetch req "foo" precheck_fail in
  (* Notify value *)
  let* () = Test_Requester.notify req P2p_peer.Id.zero "foo" 1 in
  let* b = Test_Requester.known req "foo" in
  if b then Test.fail "fetched value is still not known" ;
  if not (is_pending f1) then Test.fail "promise is still pending" ;
  unit

(** Notifies a value with a valid value. The memory table should be
    updated, the promise resolved.
*)
let test_full_requester_test_notify_valid () =
  Test.register ~__FILE__ ~title:"test notify: valid" ~tags @@ fun () ->
  let req = init_full_requester () in
  (* Fetch valid value  *)
  let f1 = Test_Requester.fetch req "foo" precheck_pass in
  (* Notify value *)
  let* () = Test_Requester.notify req P2p_peer.Id.zero "foo" 1 in
  let* b = Test_Requester.known req "foo" in
  if not b then Test.fail "fetched value is now known" ;
  let* () = Lwt.pause () (* Ensure that [f1] is scheduled *) in
  if not (is_resolved f1) then Test.fail "promise is resolved" ;
  unit

(** Notifies a value that has not been fetched. The notification is
    simply ignored, and value remains unknown.
*)
let test_full_requester_test_notify_unfetched () =
  Test.register ~__FILE__ ~title:"test notify: unfetched" ~tags @@ fun () ->
  let req = init_full_requester () in
  (* Notify value that has not been fetched *)
  let* () = Test_Requester.notify req P2p_peer.Id.zero "foo" 1 in
  let* b = Test_Requester.known req "foo" in
  if b then Test.fail "fetched value is not known" ;
  unit

(** Notifies a value that is already on disk. The notification should
    be ignored (not sure how to test this, but this code runs through
    that code path).  *)
let test_full_requester_test_notify_disk_duplicate () =
  Test.register ~__FILE__ ~title:"test notify: disk duplicate" ~tags
  @@ fun () ->
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
let test_full_requester_test_notify_memory_duplicate () =
  Test.register ~__FILE__ ~title:"test notify: memory duplicate" ~tags
  @@ fun () ->
  let req = init_full_requester () in
  (* Put value in memory *)
  let* _ = Test_Requester.inject req "foo" 1 in
  (* Fetch valid value  *)
  ignore (Test_Requester.fetch req "foo" precheck_pass) ;
  (* Notify the value *)
  Test_Requester.notify req P2p_peer.Id.zero "foo" 1

(** Test pending requests *)
let tags = common_tags

(** Notifies a value that has not been fetched. The notification should
    be ignored.
*)
let test_full_requester_test_pending_requests () =
  Test.register ~__FILE__ ~title:"test pending_requests" ~tags @@ fun () ->
  let req = init_full_requester () in
  let check_pending_count error_msg count =
    Check.(
      (Test_Requester.pending_requests req = count) int ~__LOC__ ~error_msg)
  in
  let with_request key k =
    Lwt.join
      [
        (let* _ = Test_Requester.fetch req key precheck_pass in
         unit);
        (* Ensure that the request is registered before [k] is
           scheduled and that enough time is given to the
           throttler. *)
        (let* () = Lwt_unix.sleep 0.1 in
         k ());
      ]
  in
  (* Variant of [with_request] for requests that are never satisfied. When [k]
     returns, the request is left pending. *)
  let with_unmet_request key k =
    Lwt.choose
      [
        (let* _ = Test_Requester.fetch req key precheck_pass in
         Test.fail "Request should not have been satisfied");
        (let* () = Lwt_unix.sleep 0.1 in
         k ());
      ]
  in
  (* Fetch value  *)
  check_pending_count "should have %R pending requests, has %L" 0 ;
  let* () =
    with_request "foo" (fun () ->
        check_pending_count "should have %R pending requests, has %L" 1 ;
        with_unmet_request "bar" (fun () ->
            check_pending_count "should have %R pending requests, has %L" 2 ;
            with_unmet_request "bar" (fun () ->
                check_pending_count
                  "should still have %R pending requests, has %L"
                  2 ;
                Test_Requester.clear_or_cancel req "foo" ;
                (* The first "foo" fetch should be resolved *)
                Lwt_unix.sleep 0.1)))
  in
  check_pending_count "should be back to %R pending requests, has %L" 1 ;
  unit

(** Test memory_table_length *)

(** Injects some values and checks the length of the memory table. *)
let test_full_requester_test_memory_table_length () =
  Test.register ~__FILE__ ~title:"test memory_table_length" ~tags @@ fun () ->
  let req = init_full_requester () in
  Check.(
    (Test_Requester.memory_table_length req = 0)
      int
      ~__LOC__
      ~error_msg:"should have %R cached values, has %L") ;
  let* _ = Test_Requester.inject req "foo" 1 in
  Check.(
    (Test_Requester.memory_table_length req = 1)
      int
      ~__LOC__
      ~error_msg:"should have %R cached values, has %L") ;
  let* _ = Test_Requester.inject req "bar" 2 in
  Check.(
    (Test_Requester.memory_table_length req = 2)
      int
      ~__LOC__
      ~error_msg:"should have %R cached values, has %L") ;
  let* _ = Test_Requester.inject req "bar" 2 in
  Check.(
    (Test_Requester.memory_table_length req = 2)
      int
      ~__LOC__
      ~error_msg:"should still have %R cached values, has %L") ;
  let* _ = Test_Requester.inject req "baz" 3 in
  Check.(
    (Test_Requester.memory_table_length req = 3)
      int
      ~__LOC__
      ~error_msg:"should now have %R cached values, has %L") ;
  unit

(** Test shutdown *)

let test_full_requester_shutdown () =
  Test.register ~__FILE__ ~title:"test shutdown" ~tags @@ fun () ->
  let req = init_full_requester () in
  Test_Requester.shutdown req

let () =
  test_full_requester () ;
  test_full_requester_create () ;
  test_full_requester_create_with_global_input () ;
  test_read_known_read_opt () ;
  test_full_requester_disk_found_value () ;
  test_full_requester_fetch_timeout () ;
  test_full_fetch_issues_request () ;
  test_clear_or_cancel_removes () ;
  test_clear_or_cancel_decrements () ;
  test_clear_or_cancel_cancels () ;
  test_pending_cancelled () ;
  test_pending_notified () ;
  test_pending_timeout () ;
  test_full_requester_test_simple_watch () ;
  test_full_requester_test_notify_non_fetched_watch () ;
  test_full_requester_test_double_watcher () ;
  test_full_requester_test_inject_memory () ;
  test_full_requester_test_inject_disk () ;
  test_full_requester_test_inject_requested () ;
  test_full_fetch_request_unrequested_peers_first () ;
  test_full_requester_test_inject () ;
  test_full_requester_test_notify_invalid () ;
  test_full_requester_test_notify_valid () ;
  test_full_requester_test_notify_unfetched () ;
  test_full_requester_test_notify_memory_duplicate () ;
  test_full_requester_test_notify_disk_duplicate () ;
  test_full_requester_test_pending_requests () ;
  test_full_requester_test_memory_table_length () ;
  test_full_requester_shutdown ()
