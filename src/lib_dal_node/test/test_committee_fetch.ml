(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Dal node committee fetch
   Invocation:   dune exec src/lib_dal_node/test/main.exe \
                  -- --file test_committee_fetch.ml
   Subject:      Unit tests for in-flight deduplication and cancellation
                 isolation in [Node_context.fetch_committees].
*)

open Node_context.Internal_for_tests

(** Concurrent calls to [apply_fetch_dedup] for the same level share a
    single underlying fetch; a call after the promise settles starts a fresh
    fetch. *)
let () =
  Test.register
    ~__FILE__
    ~title:
      "committee fetch: concurrent fetches for same level share one promise"
    ~tags:["dal"; "committee"; "fetch"; "dedup"]
  @@ fun () ->
  let in_flight = create_fetch_in_flight () in
  let call_count = ref 0 in
  (* Use a pending promise so that the in-flight entry is not cleared before
     the second call is made. *)
  let (pending : Committee_cache.committee tzresult Lwt.t), resolver =
    Lwt.task ()
  in
  let mock_fetch _level =
    incr call_count ;
    pending
  in
  (* Two concurrent calls while the promise is still pending — only one
     underlying fetch. *)
  let _p1 = apply_fetch_dedup ~in_flight ~fetch_fn:mock_fetch 42l in
  let _p2 = apply_fetch_dedup ~in_flight ~fetch_fn:mock_fetch 42l in
  Check.(!call_count = 1)
    Check.int
    ~error_msg:"expected 1 underlying fetch before resolution, got %R" ;
  (* Resolve the promise; Lwt.on_any removes the in-flight entry. *)
  Lwt.wakeup_later resolver (Ok Signature.Public_key_hash.Map.empty) ;
  (* Yield so that the on_any callback fires and clears the table. *)
  let* () = Lwt.pause () in
  (* A call after resolution must start a fresh fetch. *)
  let _p3 = apply_fetch_dedup ~in_flight ~fetch_fn:mock_fetch 42l in
  Check.(!call_count = 2)
    Check.int
    ~error_msg:"expected 2 fetches total after first settled, got %R" ;
  unit

(** Cancelling one waiter's handle does not cancel the shared underlying fetch
    or affect other concurrent waiters. *)
let () =
  Test.register
    ~__FILE__
    ~title:
      "committee fetch: cancelling one waiter does not cancel the shared fetch"
    ~tags:["dal"; "committee"; "fetch"; "dedup"; "cancel"]
  @@ fun () ->
  let in_flight = create_fetch_in_flight () in
  let (pending : Committee_cache.committee tzresult Lwt.t), resolver =
    Lwt.task ()
  in
  let mock_fetch _level = pending in
  let p1 = apply_fetch_dedup ~in_flight ~fetch_fn:mock_fetch 42l in
  let p2 = apply_fetch_dedup ~in_flight ~fetch_fn:mock_fetch 42l in
  Lwt.cancel p1 ;
  Lwt.wakeup_later resolver (Ok Signature.Public_key_hash.Map.empty) ;
  let* () = Lwt.pause () in
  (match Lwt.state p2 with
  | Return (Ok _) -> ()
  | Return (Error _) -> Test.fail "p2 resolved with Error"
  | Fail exn ->
      Test.fail "p2 failed with exception: %s" (Printexc.to_string exn)
  | Sleep -> Test.fail "p2 still pending after fetch resolved") ;
  unit
