(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Workers
    Invocation:   dune exec src/lib_workers/test/main.exe \
                  -- --file test_workers_unit.ml
    Subject:      Unit tests for [Worker]
*)

module Assert = Assert
open Mocked_worker
module Event = Internal_event.Simple

type error += TzCrashError

exception RaisedExn

let create_handlers (type a) ?on_completion ?on_error_extra ?on_close_extra () =
  (module struct
    type self = a Worker.t

    let on_request : type r request_error.
        self -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
        =
     fun _w request ->
      let open Lwt_result_syntax in
      match request with
      | Request.RqA _i -> return_unit
      | Request.RqB -> return_unit
      | Request.RqErr Crash -> Lwt.return_error `CrashError
      | Request.RqErr Simple -> Lwt.return_error `SimpleError
      | Request.RqErr RaiseExn -> raise RaisedExn
      | Request.RqErr Shutdown -> Lwt.return_error `Shutdown

    type launch_error = error trace

    let on_launch _w _name _param =
      let open Lwt_result_syntax in
      return (ref [])

    let on_error (type a b) w _st (r : (a, b) Request.t) (errs : b) :
        [`Continue | `Shutdown] tzresult Lwt.t =
      let open Lwt_result_syntax in
      let () = match on_error_extra with Some f -> f () | None -> () in
      let history = Worker.state w in
      let return_continue = return `Continue in
      match r with
      | Request.RqA _ -> return_continue
      | Request.RqB -> return_continue
      | Request.RqErr _ -> (
          match errs with
          | `CrashError -> Lwt.return_error [TzCrashError]
          | `SimpleError ->
              history := "RqErr" :: !history ;
              return_continue
          | `Shutdown -> return `Shutdown)

    let on_completion w r _ _st =
      let open Lwt_syntax in
      let history = Worker.state w in
      let () =
        match Request.view r with
        | Request.View (RqA i) ->
            history := Format.sprintf "RqA %d" i :: !history
        | View RqB -> history := "RqB" :: !history
        | View (RqErr _) -> ()
      in
      let* () =
        match on_completion with Some f -> f () | None -> Lwt.return_unit
      in
      Lwt.return_unit

    let on_no_request _ = Lwt.return_unit

    let on_close _w =
      let open Lwt_syntax in
      let* () =
        match on_close_extra with Some f -> f () | None -> Lwt.return_unit
      in
      Lwt.return_unit
  end : Worker.HANDLERS
    with type self = a Worker.t
     and type launch_error = error trace)

let create table handlers ?timeout name =
  Worker.launch ?timeout table name 0 handlers

let create_queue ?on_completion ?on_error_extra ?on_close_extra =
  let table = Worker.create_table Queue in
  create
    table
    (create_handlers ?on_completion ?on_error_extra ?on_close_extra ())

let create_bounded ?on_completion =
  let table = Worker.create_table (Bounded {size = 2}) in
  create table (create_handlers ?on_completion ())

let create_dropbox ?on_completion =
  let table =
    let open Worker in
    let merge _w (Any_request (neu, neu_metadata)) (old : _ option) =
      let (Any_request (r, metadata)) =
        match (neu, old) with
        | RqA i1, Some (Any_request (RqA i2, i2_metadata)) ->
            Any_request (RqA (i1 + i2), i2_metadata)
        | (RqA _ as rqa), _ -> Any_request (rqa, neu_metadata)
        | _, Some (Any_request ((RqA _ as rqa), metadata)) ->
            Any_request (rqa, metadata)
        | RqB, _ -> Any_request (neu, neu_metadata)
        | RqErr _, _ -> Any_request (neu, neu_metadata)
      in
      Some (Worker.Any_request (r, metadata))
    in
    Worker.create_table (Dropbox {merge})
  in
  create table (create_handlers ?on_completion ())

(* Simulates a stream of data with a blocking [pop] function.
   Items are buffered so pushes are never lost regardless of whether the
   worker has reached its [read] call yet.  This is necessary because
   [Lwt.pause ()] in [worker.ml]'s [loop ()] means the worker may not be
   waiting on the condition at the moment the test broadcasts. *)
let make_stream () =
  let buf = Queue.create () in
  let avail = Lwt_condition.create () in
  let push x =
    Queue.push x buf ;
    Lwt_condition.broadcast avail ()
  in
  let rec read () =
    let open Lwt_syntax in
    if Queue.is_empty buf then
      let* () = Lwt_condition.wait avail in
      read ()
    else Lwt.return (Queue.pop buf)
  in
  (push, read)

let create_callback_worker ?on_completion ?on_error_extra ?on_close_extra read =
  let table = Worker.create_table (Callback read) in
  create
    table
    (create_handlers ?on_completion ?on_error_extra ?on_close_extra ())

open Mocked_worker.Request

type abs

type _ box = Box : (unit, _) Request.t -> abs box

let build_expected_history l =
  let rec loop acc l =
    match l with
    | [] -> acc
    | Box (RqA i) :: t -> loop (Format.sprintf "RqA %d" i :: acc) t
    | Box RqB :: t -> loop ("RqB" :: acc) t
    | Box (RqErr Simple) :: t -> loop ("RqErr" :: acc) t
    | Box (RqErr _) :: _ -> acc
  in
  loop [] l

let assert_status w expected_status =
  let status_str =
    match Worker.status w with
    | Launching _ -> "Launching"
    | Running _ -> "Running"
    | Closing _ -> "Closing"
    | Closed _ -> "Closed"
  in
  Assert.assert_true
    (Format.asprintf "Worker should be of status %s" expected_status)
    (status_str = expected_status)

open Qcheck2_helpers

module Generators = struct
  open QCheck2

  let request =
    let open Gen in
    oneof
      [
        Gen.map (fun i -> Box (RqA i)) int;
        return (Box RqB);
        return (Box (RqErr Simple));
      ]

  let request_series =
    let open Gen in
    small_list (small_list request)
end

let get_history w =
  let history = Worker.state w in
  !history

(* General function to send requests
 * and build the expected history *)
let push_multiple_requests w l =
  let open Lwt_syntax in
  let history = ref [] in
  let* () =
    Lwt.catch
      (fun () ->
        List.iter_s
          (fun (Box r) ->
            let open Lwt_syntax in
            let* _ = Worker.Queue.push_request_and_wait w r in
            history := get_history w ;
            Lwt.return_unit)
          l)
      (fun _ -> Lwt.return_unit)
  in
  return !history

let print_list l = Format.sprintf "[%s]" (String.concat ", " l)

let test_random_requests create_queue =
  let open QCheck2 in
  Test.make ~name:"Non-failing requests" Generators.request_series
  @@ fun series ->
  let actual_t =
    List.map_s
      (fun l ->
        let open Lwt_syntax in
        let* w = create_queue "random_worker" in
        match w with
        | Ok w ->
            let* r = push_multiple_requests w l in
            let* () = Worker.shutdown w in
            return r
        | Error _ -> Lwt.return_nil)
      series
  in
  let actual = Lwt_main.run actual_t in
  let expected = List.map build_expected_history series in
  let pp fmt l =
    Format.fprintf fmt "%s" (print_list @@ List.map print_list l)
  in
  qcheck_eq' ~expected ~actual ~eq:( = ) ~pp ()

let assert_history actual expected =
  Assert.assert_true
    (Format.asprintf
       "History should be the same as expected: actual %s vs expected %s"
       (print_list actual)
       (print_list expected))
    (actual = expected)

let push_and_assert_history w l =
  let open Lwt_syntax in
  let expected = build_expected_history l in
  let* actual = push_multiple_requests w l in
  assert_history actual expected ;
  return_unit

(** Run [f ()] under a [SIGALRM] watchdog that calls [exit 1] with [msg] if
    [seconds] elapse before [f] completes.  Necessary because [Lwt_unix.sleep]
    cannot fire when the Lwt scheduler is starved. *)
let with_alarm seconds msg f =
  let open Lwt_result_syntax in
  let old_handler =
    Sys.signal
      Sys.sigalrm
      (Sys.Signal_handle
         (fun _ ->
           Printf.eprintf "\nFAIL %s\n%!" msg ;
           exit 1))
  in
  ignore (Unix.alarm seconds) ;
  let* result = f () in
  ignore (Unix.alarm 0) ;
  Sys.set_signal Sys.sigalrm old_handler ;
  return result

(* Checks a non crashing request actually doesn't make the worker crash *)
let test_push_crashing_request () =
  with_alarm 2 "test_push_crashing_request: timed out" @@ fun () ->
  let open Lwt_result_syntax in
  let* w = create_queue "crashing_worker" in
  assert_status w "Running" ;
  let*! () =
    push_and_assert_history w [Box RqB; Box RqB; Box (RqErr Crash); Box RqB]
  in
  assert_status w "Closed" ;
  return_unit

(* Checks status is Closed when worker is has been shutdown and that accessing
   the state raises an exception *)
let test_cancel_worker () =
  with_alarm 2 "test_cancel_worker: timed out" @@ fun () ->
  let open Lwt_result_syntax in
  let* w = create_queue "canceled_worker" in
  let*! () = push_and_assert_history w [Box RqB] in
  assert_status w "Running" ;
  let*! () = Worker.shutdown w in
  let state_not_available =
    try
      let _ = Worker.state w in
      false
    with Invalid_argument _ -> true
  in
  Assert.assert_true
    (Format.asprintf "State should not be available")
    state_not_available ;
  assert_status w "Closed" ;
  return_unit

let test_shutdown_on_error () =
  with_alarm 2 "test_shutdown_on_error: timed out" @@ fun () ->
  let open Lwt_result_syntax in
  let* w = create_queue "shutdown_worker" in
  assert_status w "Running" ;
  let*! () =
    push_and_assert_history w [Box RqB; Box RqB; Box (RqErr Shutdown); Box RqB]
  in
  assert_status w "Closed" ;
  return_unit

(** Regression test for d31bdd72.
    A Callback worker whose [on_error] returns [`Continue] on a read that
    resolves immediately (simulating a TCP fd that has already been closed)
    must not starve the Lwt scheduler: [Worker.shutdown] must complete in
    bounded time.

    Without the [Lwt.pause ()] at the top of [loop ()] in [worker.ml],
    the loop spins without ever yielding, and [Worker.shutdown] never gets
    scheduled.

    Because scheduler starvation also prevents [Lwt_unix.sleep] from firing,
    the watchdog uses [Unix.alarm] / [SIGALRM] — an OS-level signal that is
    delivered regardless of the Lwt scheduler state. *)
let test_shutdown_after_continue_spin () =
  let open Lwt_result_syntax in
  (* [read] resolves immediately on every call, simulating reads on an
     already-closed TCP file-descriptor.  The request type [RqErr Simple]
     causes [on_error] to return [`Continue] without yielding. *)
  let read () =
    Lwt.return Worker.(Any_request (RqErr Simple, {scope = None}))
  in
  with_alarm 2 "test_shutdown_after_continue_spin: timed out" @@ fun () ->
  let* w = create_callback_worker read "spin_worker" in
  (* Yield once so the callback loop has a chance to start spinning. *)
  let*! () = Lwt.pause () in
  (* Worker.shutdown must complete before SIGALRM fires. *)
  let*! () = Worker.shutdown w in
  return_unit

(** Regression test for d31bdd72 — success path.
    A Callback worker whose [read] resolves immediately with a successful
    request, and whose [on_request] and [on_completion] also resolve
    synchronously, loops via the success path: [read] → [on_request Ok] →
    [on_completion] → [loop ()].  Without [Lwt.pause ()] at the top of
    [loop ()], this path starves the Lwt scheduler just as the [Continue]
    path does.

    Uses the same [Unix.alarm] / SIGALRM watchdog as
    [test_shutdown_after_continue_spin]. *)
let test_shutdown_after_success_spin () =
  let open Lwt_result_syntax in
  (* [read] always resolves immediately with a successful request, so
     [on_request] takes the [Ok ()] → [loop ()] path on every iteration. *)
  let read () = Lwt.return Worker.(Any_request (RqA 0, {scope = None})) in
  with_alarm 2 "test_shutdown_after_success_spin: timed out" @@ fun () ->
  let* w = create_callback_worker read "success_spin_worker" in
  (* Yield once so the callback loop has a chance to start. *)
  let*! () = Lwt.pause () in
  (* Worker.shutdown must complete before SIGALRM fires. *)
  let*! () = Worker.shutdown w in
  return_unit

(** The Lwt scheduler must remain fair while a sync-read Callback worker is
    looping.  A background task that yields [n] times and increments a counter
    must complete while the worker is running.

    Without [Lwt.pause ()] in [loop ()], the worker monopolises the scheduler
    and the background task never makes progress. *)
let test_scheduler_fairness () =
  let open Lwt_result_syntax in
  let n = 10 in
  let count = ref 0 in
  let rec background () =
    let open Lwt_syntax in
    if !count >= n then Lwt.return_unit
    else
      let* () = Lwt.pause () in
      incr count ;
      background ()
  in
  let read () = Lwt.return Worker.(Any_request (RqA 0, {scope = None})) in
  with_alarm
    2
    "test_scheduler_fairness: background task did not complete — worker \
     monopolised the Lwt scheduler (regression of d31bdd72)"
    (fun () ->
      let* w = create_callback_worker read "fairness_worker" in
      let*! () = background () in
      Assert.assert_true
        (Format.sprintf
           "background task must complete n=%d iterations (got %d)"
           n
           !count)
        (!count = n) ;
      let*! () = Worker.shutdown w in
      return_unit)

(** Two concurrent sync-read Callback workers must both be shutdownable.

    Without [Lwt.pause ()], a single spinning worker starves the entire
    scheduler — a second worker (and its shutdown promise) can never run. *)
let test_two_concurrent_spin_workers () =
  let open Lwt_result_syntax in
  let read () =
    Lwt.return Worker.(Any_request (RqErr Simple, {scope = None}))
  in
  with_alarm
    2
    "test_two_concurrent_spin_workers: Worker.shutdown timed out — one worker \
     starved the other (regression of d31bdd72)"
    (fun () ->
      let* w1 = create_callback_worker read "spin_worker_1" in
      let* w2 = create_callback_worker read "spin_worker_2" in
      let*! () = Lwt.pause () in
      let*! () = Lwt.join [Worker.shutdown w1; Worker.shutdown w2] in
      return_unit)

(** A Callback worker that returns [Continue] on errors must still process
    subsequent successful requests correctly.

    Verifies that the worker is not left in a broken state after N error
    iterations and that history is recorded correctly for both paths. *)
let test_continue_then_success () =
  with_alarm 2 "test_continue_then_success: timed out" @@ fun () ->
  let open Lwt_result_syntax in
  let n_errors = 5 in
  let t_done, u_done = Lwt.task () in
  let push, read = make_stream () in
  let* w =
    create_callback_worker
      ~on_completion:(fun () ->
        Lwt.wakeup_later u_done () ;
        Lwt.return_unit)
      read
      "continue_then_success_worker"
  in
  (* Push n error requests that trigger the Continue path, then one success. *)
  for _ = 1 to n_errors do
    push Worker.(Any_request (RqErr Simple, {scope = None}))
  done ;
  push Worker.(Any_request (RqA 42, {scope = None})) ;
  (* Wait for the successful request to complete. *)
  let*! () = t_done in
  let actual = get_history w in
  let error_boxes =
    let rec loop acc n =
      if n = 0 then acc else loop (Box (RqErr Simple) :: acc) (n - 1)
    in
    loop [] n_errors
  in
  let expected = build_expected_history (error_boxes @ [Box (RqA 42)]) in
  assert_history actual expected ;
  let*! () = Worker.shutdown w in
  return_unit

(** Deactivate-once guard: a resource deactivated in both [on_error] (protocol
    error path) and [on_close] must be deactivated exactly once regardless of
    which path fires first.

    Mirrors the [deactivate_once] pattern added to [p2p_reader.ml]. *)

(** Sub-case A: [on_error] fires first (crash/protocol-error path).
    [on_error] deactivates and returns an error → worker shuts down →
    [on_close] attempts deactivation again → guarded no-op.
    Expected: deactivation count = 1. *)
let test_deactivate_once_on_error_path () =
  with_alarm 2 "test_deactivate_once_on_error_path: timed out" @@ fun () ->
  let open Lwt_result_syntax in
  let count = ref 0 in
  let activated = ref false in
  let deactivate_once () =
    if not !activated then (
      activated := true ;
      incr count)
  in
  let* w =
    create_queue
      ~on_error_extra:(fun () -> deactivate_once ())
      ~on_close_extra:(fun () ->
        deactivate_once () ;
        Lwt.return_unit)
      "deactivate_once_error_worker"
  in
  (* RqErr Crash: on_error returns Lwt.return_error → worker crashes →
     on_close is called → deactivate_once is a no-op. *)
  let*! _ = Worker.Queue.push_request_and_wait w (RqErr Crash) in
  Assert.assert_true
    "deactivate should be called exactly once when on_error fires first"
    (!count = 1) ;
  return_unit

(** Sub-case B: only [on_close] fires (normal shutdown path).
    Worker shuts down without an error → [on_close] deactivates.
    Expected: deactivation count = 1. *)
let test_deactivate_once_on_close_path () =
  with_alarm 2 "test_deactivate_once_on_close_path: timed out" @@ fun () ->
  let open Lwt_result_syntax in
  let count = ref 0 in
  let activated = ref false in
  let deactivate_once () =
    if not !activated then (
      activated := true ;
      incr count)
  in
  let* w =
    create_queue
      ~on_close_extra:(fun () ->
        deactivate_once () ;
        Lwt.return_unit)
      "deactivate_once_close_worker"
  in
  let*! () = Worker.shutdown w in
  Assert.assert_true
    "deactivate should be called exactly once on normal shutdown"
    (!count = 1) ;
  return_unit

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3004
   in follow-up MR: fix the handling of exceptions and
   integrate this test *)
let _test_raise_exn () =
  let open Lwt_result_syntax in
  let* w = create_queue "exn_worker" in
  assert_status w "Running" ;
  let*! _ = Worker.Queue.push_request_and_wait w (RqErr RaiseExn) in
  (* Define the right behavior when exception is raised *)
  return_unit

(** On dropbox mode, checks that asynchronous requests are merged when the
    worker is busy processing an earlier request.

    The test uses [t_processing] as a handshake: the worker fires it (via
    [Lwt.wakeup_later]) from inside the first [on_completion] callback,
    signalling that it has taken the first request and the dropbox slot is
    free.  Only then does the test push the [n] additional requests, which
    are guaranteed to land on an empty slot and be merged into one [RqA n].

    This is robust regardless of how many [Lwt.pause ()] steps separate
    worker launch from the worker's first [Lwt_dropbox.take]. *)
let test_async_dropbox () =
  with_alarm 2 "test_async_dropbox: timed out" @@ fun () ->
  let open Lwt_result_syntax in
  let t_end, u_end = Lwt.task () in
  (* Fired by the first [on_completion] to tell the test the dropbox slot
     is free (worker has taken the first request). *)
  let t_processing, u_processing = Lwt.task () in
  let nb_completion = ref 0 in
  let* w =
    create_dropbox
      ~on_completion:(fun () ->
        incr nb_completion ;
        if !nb_completion = 1 then Lwt.wakeup_later u_processing () ;
        if !nb_completion = 2 then Lwt.wakeup_later u_end () ;
        Lwt.return_unit)
      "dropbox_worker"
  in
  let rq = RqA 1 in
  let n = 10 in
  (* Send the first request. *)
  Worker.Dropbox.put_request w rq ;
  (* Wait until the worker is inside [on_completion] for the first request:
     at that point the dropbox slot is free. *)
  let*! () = t_processing in
  (* Queue [n] more requests while the worker is still processing the first.
     They all land on the free slot and are merged into a single [RqA n]. *)
  for _i = 1 to n do
    Worker.Dropbox.put_request w rq
  done ;
  let*! () = t_end in
  (* Expected: two completions — [RqA 1] first, then the merged [RqA n]. *)
  let expected = build_expected_history [Box (RqA 1); Box (RqA n)] in
  let actual = get_history w in
  assert_history actual expected ;
  return_unit

let test_callback_worker () =
  with_alarm 2 "test_callback_worker: timed out" @@ fun () ->
  let open Lwt_result_syntax in
  let nb_requests = 10 in
  let nb_completions = ref 0 in
  (* To ensure the worker has handled all the requests before testing its
     history, we start a promise that will be resolved only once all the
     requests have been completed. *)
  let t_end, u_end = Lwt.task () in
  (* The worker doesn't have a request buffer: it will take a function that
     tries to read a value from somewhere, and is expected to block until
     something is read. *)
  let push_to_stream, read_from_stream = make_stream () in
  let* w =
    create_callback_worker
      ~on_completion:(fun () ->
        incr nb_completions ;
        if !nb_completions = nb_requests then Lwt.wakeup u_end () ;
        Lwt.return_unit)
      read_from_stream
      "stream_worker"
  in
  (* Initially the history should be empty. *)
  let initial_history = get_history w in
  assert_history initial_history [] ;
  (* Now, we prepare a set of request that is pushed to a queue that is not
     handled by the worker. It will only access to the `read` function. *)
  let requests = Stdlib.List.init nb_requests (fun i -> RqA i) in
  List.iter
    (fun req -> push_to_stream Worker.(Any_request (req, {scope = None})))
    requests ;
  (* Wait for all the requests to be handled. *)
  let*! () = t_end in
  let expected =
    build_expected_history @@ List.map (fun req -> Box req) requests
  in
  let actual = get_history w in
  assert_history actual expected ;
  return_unit

let wrap_qcheck test () =
  let _ = QCheck_alcotest.to_alcotest test in
  Lwt_result_syntax.return_unit

let tests_history =
  ( "Queue history",
    [
      Tztest.tztest
        "Random normal requests"
        `Quick
        (wrap_qcheck (test_random_requests create_queue));
      Tztest.tztest
        "Random normal requests on Bounded"
        `Quick
        (wrap_qcheck (test_random_requests create_bounded));
    ] )

let tests_status =
  ( "Status",
    [
      Tztest.tztest "Canceled worker" `Quick test_cancel_worker;
      Tztest.tztest "Crashing requests" `Quick test_push_crashing_request;
      Tztest.tztest "Shutdown on error" `Quick test_shutdown_on_error;
      Tztest.tztest
        "Shutdown after Continue spin (regression d31bdd72)"
        `Quick
        test_shutdown_after_continue_spin;
      Tztest.tztest
        "Shutdown after success spin (regression d31bdd72)"
        `Quick
        test_shutdown_after_success_spin;
    ] )

let tests_buffer =
  ("Buffer handling", [Tztest.tztest "Dropbox/Async" `Quick test_async_dropbox])

let tests_external_message =
  ( "External messages",
    [
      Tztest.tztest "Callback" `Quick test_callback_worker;
      Tztest.tztest "Continue then success" `Quick test_continue_then_success;
    ] )

let tests_scheduler =
  ( "Scheduler guarantees",
    [
      Tztest.tztest "Fairness" `Quick test_scheduler_fairness;
      Tztest.tztest
        "Two concurrent spin workers"
        `Quick
        test_two_concurrent_spin_workers;
    ] )

let tests_lifecycle =
  ( "Lifecycle hooks",
    [
      Tztest.tztest
        "Deactivate once — on_error path"
        `Quick
        test_deactivate_once_on_error_path;
      Tztest.tztest
        "Deactivate once — on_close path"
        `Quick
        test_deactivate_once_on_close_path;
    ] )

let () =
  Alcotest_lwt.run
    ~__FILE__
    "Workers"
    [
      tests_history;
      tests_status;
      tests_buffer;
      tests_external_message;
      tests_scheduler;
      tests_lifecycle;
    ]
  |> Lwt_main.run
