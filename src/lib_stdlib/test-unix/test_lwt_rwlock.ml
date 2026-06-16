(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    _______

    Invocation: dune exec src/lib_stdlib/test-unix/main.exe \
                  -- --file test_lwt_rwlock.ml
 *)

open Lwt.Syntax

type lock_access = Read | Write

type event_action = Acquired | Released

type event = {access : lock_access; action : event_action; thread_id : string}

let equal_event a b =
  a.access = b.access && a.action = b.action && a.thread_id = b.thread_id

let pp_event fmt {access; action; thread_id} =
  let action_s =
    match action with Acquired -> "acquired" | Released -> "released"
  in
  let access_s = match access with Read -> "Read" | Write -> "Write" in
  Format.fprintf fmt "{%s %s %s}" thread_id action_s access_s

let ev thread_id action access = {access; action; thread_id}

(* Instrumented Lwt_rwlock with events tracking *)
module Lwt_rwlock = struct
  type t = {lock : Lwt_rwlock.t; mutable events : event list}

  let create () = {lock = Lwt_rwlock.create (); events = []}

  let add_event t thread_id action access =
    t.events <- ev thread_id action access :: t.events ;
    Log.debug "%a" pp_event (ev thread_id action access)

  let read_lock name t =
    Log.debug "[%s waiting Read]" name ;
    let* () = Lwt_rwlock.read_lock t.lock in
    add_event t name Acquired Read ;
    unit

  let read_unlock name t =
    Lwt_rwlock.read_unlock t.lock ;
    add_event t name Released Read

  let write_lock name t =
    Log.debug "[%s waiting Write]" name ;
    let* () = Lwt_rwlock.write_lock t.lock in
    add_event t name Acquired Write ;
    unit

  let write_unlock name t =
    Lwt_rwlock.write_unlock t.lock ;
    add_event t name Released Write

  let with_read_lock name t f =
    Log.debug "[%s waiting Read]" name ;
    let* () =
      Lwt_rwlock.with_read_lock t.lock @@ fun () ->
      add_event t name Acquired Read ;
      f ()
    in
    add_event t name Released Read ;
    unit

  let with_write_lock name t f =
    Log.debug "[%s waiting Write]" name ;
    let* () =
      Lwt_rwlock.with_write_lock t.lock @@ fun () ->
      add_event t name Acquired Write ;
      f ()
    in
    add_event t name Released Write ;
    unit

  let events t = List.rev t.events
end

(** Test that multiple readers can acquire the lock concurrently *)
let test_multiple_readers () =
  let lock = Lwt_rwlock.create () in
  let readers_active = ref 0 in
  let max_concurrent_readers = ref 0 in
  let reader () =
    Lwt_rwlock.with_read_lock "reader" lock (fun () ->
        incr readers_active ;
        max_concurrent_readers := max !max_concurrent_readers !readers_active ;
        let* () = Lwt.pause () in
        let* () = Lwt.pause () in
        decr readers_active ;
        unit)
  in
  (* Start 5 readers concurrently *)
  let* () = Lwt.join (List.init 5 (fun _ -> reader ())) in
  (* All 5 should have been active at the same time *)
  Assert.Int.equal
    ~loc:__LOC__
    ~msg:"All readers should run concurrently"
    5
    !max_concurrent_readers ;
  unit

(** Test writer priority and exclusive access: when a writer is waiting, new
    readers block until the writer has acquired and released the lock. Also
    verifies that the writer does not overlap with any reader. *)
let test_writer_priority () =
  let lock = Lwt_rwlock.create () in
  (* reader1 acquires lock *)
  let* () = Lwt_rwlock.read_lock "reader1" lock in
  (* writer tries to acquire - will block and set writer_waiting *)
  let writer_done =
    let* () = Lwt_rwlock.write_lock "writer" lock in
    let* () = Lwt.pause () in
    Lwt_rwlock.write_unlock "writer" lock ;
    unit
  in
  (* Give writer a chance to register as waiting *)
  let* () = Lwt.pause () in
  (* reader2 tries to acquire - should block because writer is waiting *)
  let reader2_done =
    let* () = Lwt_rwlock.read_lock "reader2" lock in
    Lwt_rwlock.read_unlock "reader2" lock ;
    unit
  in
  (* Give reader2 a chance to try acquiring *)
  let* () = Lwt.pause () in
  (* Release reader1 *)
  Lwt_rwlock.read_unlock "reader1" lock ;
  (* Wait for everything to complete *)
  let* () = Lwt.join [writer_done; reader2_done] in
  (* Writer gets priority over reader2, and does not overlap with either
     reader *)
  Assert.equal_list
    ~eq:equal_event
    ~pp:pp_event
    ~loc:__LOC__
    ~msg:"Writer should get priority over reader2"
    [
      ev "reader1" Acquired Read;
      ev "reader1" Released Read;
      ev "writer" Acquired Write;
      ev "writer" Released Write;
      ev "reader2" Acquired Read;
      ev "reader2" Released Read;
    ]
    (Lwt_rwlock.events lock) ;
  unit

(** Test that with_read_lock releases lock even on exception *)
let test_with_read_lock_exception () =
  let lock = Lwt_rwlock.create () in
  let exn_raised = ref false in
  let* () =
    Lwt.catch
      (fun () ->
        Lwt_rwlock.with_read_lock "reader" lock (fun () ->
            exn_raised := true ;
            failwith "test exception"))
      (fun _ -> unit)
  in
  Assert.is_true
    ~loc:__LOC__
    ~msg:"Exception should have been raised"
    !exn_raised ;
  (* Lock should be released - writer should be able to acquire immediately *)
  let acquired = ref false in
  let writer =
    let* () = Lwt_rwlock.write_lock "writer" lock in
    acquired := true ;
    Lwt_rwlock.write_unlock "writer" lock ;
    unit
  in
  let* () = Lwt.pause () in
  let* () = writer in
  Assert.is_true
    ~loc:__LOC__
    ~msg:"Writer should acquire lock after read_lock exception"
    !acquired ;
  unit

(** Test that with_write_lock releases lock even on exception *)
let test_with_write_lock_exception () =
  let lock = Lwt_rwlock.create () in
  let exn_raised = ref false in
  let* () =
    Lwt.catch
      (fun () ->
        Lwt_rwlock.with_write_lock "writer" lock (fun () ->
            exn_raised := true ;
            failwith "test exception"))
      (fun _ -> unit)
  in
  Assert.is_true
    ~loc:__LOC__
    ~msg:"Exception should have been raised"
    !exn_raised ;
  (* Lock should be released - reader should be able to acquire immediately *)
  let acquired = ref false in
  let reader =
    let* () = Lwt_rwlock.read_lock "reader" lock in
    acquired := true ;
    Lwt_rwlock.read_unlock "reader" lock ;
    unit
  in
  let* () = Lwt.pause () in
  let* () = reader in
  Assert.is_true
    ~loc:__LOC__
    ~msg:"Reader should acquire lock after write_lock exception"
    !acquired ;
  unit

(** Test basic read-write-read sequence *)
let test_basic_sequence () =
  let lock = Lwt_rwlock.create () in
  let value = ref 0 in
  (* Reader sees initial value *)
  let* () =
    Lwt_rwlock.with_read_lock "reader1" lock (fun () ->
        Assert.Int.equal ~loc:__LOC__ ~msg:"Initial value should be 0" 0 !value ;
        unit)
  in
  (* Writer updates value *)
  let* () =
    Lwt_rwlock.with_write_lock "writer" lock (fun () ->
        value := 42 ;
        unit)
  in
  (* Reader sees updated value *)
  let* () =
    Lwt_rwlock.with_read_lock "reader2" lock (fun () ->
        Assert.Int.equal
          ~loc:__LOC__
          ~msg:"Reader should see updated value"
          42
          !value ;
        unit)
  in
  unit

(** Test that writer waits for all readers to finish *)
let test_writer_waits_for_readers () =
  let lock = Lwt_rwlock.create () in
  let reader_count = ref 0 in
  (* Start multiple readers that hold the lock for a while *)
  let readers =
    List.init 3 (fun i ->
        Lwt_rwlock.with_read_lock (sf "reader%d" i) lock @@ fun () ->
        incr reader_count ;
        (* Simulate some work by yielding *)
        let* () = Lwt.pause () in
        let* () = Lwt.pause () in
        let* () = Lwt.pause () in
        decr reader_count ;
        unit)
  in
  (* Give readers time to acquire lock *)
  let* () = Lwt.pause () in
  Assert.Int.gt
    ~loc:__LOC__
    ~msg:"Readers should have acquired lock"
    !reader_count
    0 ;
  (* Writer tries to acquire *)
  let writer =
    Lwt_rwlock.with_write_lock "writer" lock @@ fun () ->
    (* When writer acquires, all readers should have finished *)
    Assert.Int.equal
      ~loc:__LOC__
      ~msg:"Writer should not see active readers %L when it acquires lock"
      !reader_count
      0 ;
    unit
  in
  Lwt.join (writer :: readers)

(** Test that two waiting writers both have priority over a late reader.
    A second waiting writer shouldn't allow a concurrent reader to take priority
    by resetting the waiting_writers flag.
    Scenario: reader holds lock, writer1 and writer2 both wait, then
    late_reader arrives.
    Expected order: reader, writer1, writer2, late_reader. *)
let test_two_writers_waiting () =
  let lock = Lwt_rwlock.create () in
  (* reader acquires read lock *)
  let* () = Lwt_rwlock.read_lock "reader" lock in
  (* writer1 tries to acquire — blocks, registers as waiting *)
  let w1_done = Lwt_rwlock.with_write_lock "writer1" lock Lwt.return in
  (* writer2 tries to acquire — also blocks *)
  let w2_done = Lwt_rwlock.with_write_lock "writer2" lock Lwt.pause in
  (* Release reader — should unblock writer1, then writer2, then
     late_reader *)
  let* () = Lwt.pause () in
  Lwt_rwlock.read_unlock "reader" lock ;
  let* () = w1_done in
  (* late_reader arrives after writer1 has unlocked and after writer2 asked to
     acquire the lock — should be blocked by writer2 *)
  let late_reader_done =
    Lwt_rwlock.with_read_lock "late_reader" lock Lwt.pause
  in
  let* () = Lwt.join [w2_done; late_reader_done] in
  (* Both writers must complete before late_reader gets in *)
  Assert.equal_list
    ~eq:equal_event
    ~pp:pp_event
    ~loc:__LOC__
    ~msg:"Both writers should complete before late_reader"
    [
      ev "reader" Acquired Read;
      ev "reader" Released Read;
      ev "writer1" Acquired Write;
      ev "writer1" Released Write;
      ev "writer2" Acquired Write;
      ev "writer2" Released Write;
      ev "late_reader" Acquired Read;
      ev "late_reader" Released Read;
    ]
    (Lwt_rwlock.events lock) ;
  unit

let tests =
  [
    ("test_multiple_readers", `Quick, test_multiple_readers);
    ("test_writer_priority", `Quick, test_writer_priority);
    ("test_with_read_lock_exception", `Quick, test_with_read_lock_exception);
    ("test_with_write_lock_exception", `Quick, test_with_write_lock_exception);
    ("test_basic_sequence", `Quick, test_basic_sequence);
    ("test_writer_waits_for_readers", `Quick, test_writer_waits_for_readers);
    ("test_two_writers_waiting", `Quick, test_two_writers_waiting);
  ]

let () =
  Lwt_main.run @@ Alcotest_lwt.run ~__FILE__ "Lwt_rwlock" [("rwlock", tests)]
