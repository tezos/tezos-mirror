(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Workers
    Invocation:   dune exec src/lib_bees/test/main.exe \
                  -- --file test_bees_unit_eio.ml
    Subject:      Unit tests for [Worker]
*)

module Assert = Assert
open Mocked_worker

module Events = struct
  let section = ["test_bees_unit_eio"]

  include Internal_event.Simple

  let request_received =
    declare_1
      ~section
      ~name:"request_received"
      ~msg:"request {req} received"
      ~level:Notice
      ~pp1:Request.pp
      ("req", Request.encoding)

  let emit event param = Tezos_bees.Hive.async_lwt (fun () -> emit event param)
end

type error += TzCrashError

exception RaisedExn

let sleep d =
  let env = Tezos_base_unix.Event_loop.env_exn () in
  Eio.Time.sleep env#clock d

let create_handlers (type a) ?on_completion ?(slow = false) () =
  (module struct
    type self = a Worker.t

    let on_request :
        type r request_error.
        self -> (r, request_error) Request.t -> (r, request_error) result =
     fun _w request ->
      let () = if slow then sleep 0.2 else () in
      let () = Events.(emit request_received) (Request.view request) in
      match request with
      | Request.RqA _i -> (Ok () : (r, request_error) result)
      | Request.RqB -> Ok ()
      | Request.RqErr Crash -> Error `CrashError
      | Request.RqErr Simple -> Error `SimpleError
      | Request.RqErr RaiseExn -> raise RaisedExn

    type launch_error = error trace

    let on_launch _w _name _param =
      let open Result_syntax in
      return (ref [])

    let on_error (type a b) w _st (r : (a, b) Request.t) (errs : b) :
        unit tzresult =
      let open Result_syntax in
      let history = Worker.state w in
      match r with
      | Request.RqA _ -> return_unit
      | Request.RqB -> return_unit
      | Request.RqErr _ -> (
          match errs with
          | `CrashError -> Error [TzCrashError]
          | `SimpleError ->
              history := "RqErr" :: !history ;
              return_unit)

    let on_completion w r _ _st =
      let history = Worker.state w in
      let () =
        match Request.view r with
        | Request.View (RqA i) ->
            history := Format.sprintf "RqA %d" i :: !history
        | View RqB -> history := "RqB" :: !history
        | View (RqErr _) -> ()
      in
      match on_completion with Some f -> f () | None -> ()

    let on_no_request _ = ()

    let on_close _w = ()
  end : Worker.EIO_HANDLERS
    with type self = a Worker.t
     and type launch_error = error trace)

let create table handlers ?(timeout : float option) name =
  let timeout =
    Option.bind timeout (fun timeout ->
        Ptime.(of_float_s timeout |> Option.map Ptime.to_span))
  in
  Worker.launch_eio ?timeout table ~name 0 handlers

let create_queue ?timeout ?on_completion =
  let table = Worker.create_table Queue in
  create ?timeout table (create_handlers ?on_completion ())

let create_bounded ?timeout ?on_completion =
  let table = Worker.create_table (Bounded {size = 2}) in
  create ?timeout table (create_handlers ?on_completion ())

let create_dropbox ?timeout ?on_completion ?slow =
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
  create ?timeout table (create_handlers ?slow ?on_completion ())

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
  Alcotest.check
    Alcotest.string
    "Worker should be of status"
    expected_status
    status_str

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
  (* Using a ref instead of calling it at the end of requests processing
     because you might want to have a failing request that would trigger
     a worker shutdown but still wanting to have the history when it
     failed. *)
  let history = ref [] in
  let () =
    try
      List.iter
        (fun (Box r) ->
          let p = Worker.Queue.push_request_and_wait_eio w r in
          match Eio.Promise.await p with
          | Error _ -> ()
          | Ok () -> history := get_history w)
        l
    with _ -> ()
  in
  !history

let print_list l = Format.sprintf "[%s]" (String.concat ", " l)

let test_random_requests create_queue =
  let open QCheck2 in
  Test.make ~name:"Non-failing requests" Generators.request_series
  @@ fun series ->
  let actual =
    List.map
      (fun l ->
        let w = create_queue "random_worker" in
        match w with
        | Ok w ->
            let r = push_multiple_requests w l in
            let () = Worker.shutdown_eio w in
            r
        | Error _ -> [])
      series
  in
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
  let expected = build_expected_history l in
  let actual = push_multiple_requests w l in
  assert_history actual expected

(* Checks a non crashing request actually doesn't make the worker crash *)
let test_push_crashing_request () =
  let open Result_syntax in
  let* w = create_queue "crashing_worker" in
  assert_status w "Running" ;
  push_and_assert_history w [Box RqB; Box RqB; Box (RqErr Crash); Box RqB] ;
  (* The worker loop run in another fiber, let the scheduler run the shutdown
     process before continuing. *)
  let () = sleep 0.1 in
  assert_status w "Closed" ;
  return_unit

(* Checks status is Closed when worker is has been shutdown and that accessing
   the state raises an exception *)
let test_cancel_worker () =
  let open Result_syntax in
  let* w = create_queue "canceled_worker" in
  assert_status w "Running" ;
  push_and_assert_history w [Box RqB] ;
  assert_status w "Running" ;
  Worker.shutdown_eio w ;
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3004
   in follow-up MR: fix the handling of exceptions and
   integrate this test *)
let _test_raise_exn () =
  let open Result_syntax in
  let* w = create_queue "exn_worker" in
  assert_status w "Running" ;
  let _ = Worker.Queue.push_request_and_wait w (RqErr RaiseExn) in
  (* Define the right behavior when exception is raised *)
  return_unit

(* On dropbox mode, checks asynchronous requests leads to merge when the
   completion takes time *)
let test_async_dropbox () =
  let open Result_syntax in
  let t_end, u_end = Eio.Promise.create ~label:"end" () in
  let t_each, u_each = Eio.Promise.create ~label:"each" () in
  let nb_completion = ref 0 in
  let* w =
    (* We want slow request in order to make sure that all the injected
       requests are merge while the first one is being treated *)
    create_dropbox
      ~slow:true
      ~on_completion:(fun () ->
        incr nb_completion ;
        let () = Eio.Promise.await t_each in
        if !nb_completion = 2 then Eio.Promise.resolve u_end () ;
        ())
      "dropbox_worker"
  in
  let rq = RqA 1 in
  let n = 10 in
  (* First request is sent *)
  Worker.Dropbox.put_request w rq ;
  (* Sleep in order to make sure that the first request is handled
     before the others one are sent *)
  sleep 0.1 ;
  (* While the first request is handled, n other requests are sent *)
  (* These requests should be merged into one *)
  for _i = 1 to n do
    Worker.Dropbox.put_request w rq
  done ;
  Eio.Promise.resolve u_each () ;
  Eio.Promise.await t_end ;
  (* Hence the expected result being two requests, the first blocking one *)
  (* and the second being the result of the merge *)
  let expected = build_expected_history [Box (RqA 1); Box (RqA n)] in
  let actual = get_history w in
  assert_history actual expected ;
  return_unit

let wrap_qcheck test () =
  let _ = QCheck_alcotest.to_alcotest test in
  Result_syntax.return_unit

(** Because our tests might be ran in the same process as other tests,
    we need to run our tests in a child process or next tests ran
    will fail if using [Unix.fork].
*)
let tztest label fn =
  Alcotest.test_case label `Quick @@ fun () ->
  match Lwt_unix.fork () with
  | 0 -> (
      (* FIXME: do we want to use [Tezos_base_unix.Event_loop.main_run_eio]?
         If so, the tests block at some point. *)
      match
        Tezos_base_unix.Event_loop.main_run ~eio:true @@ fun () ->
        Lwt_eio.run_eio fn
      with
      | Ok () -> exit 0
      | Error _ -> exit 1)
  | pid -> (
      let _, status = Unix.waitpid [] pid in
      match status with
      | Unix.WEXITED 0 -> ()
      | _ ->
          let msg = Format.sprintf "Error in %s." label in
          raise (Alcotest.fail msg))

let tests_history =
  ( "Queue history",
    [
      tztest
        "Random normal requests (eio handlers)"
        (wrap_qcheck (test_random_requests create_queue));
      tztest
        "Random normal requests on Bounded (eio handlers)"
        (wrap_qcheck (test_random_requests create_bounded));
    ] )

let tests_status =
  ( "Status",
    [
      tztest "Canceled worker (eio handlers)" test_cancel_worker;
      tztest "Crashing requests (eio handlers)" test_push_crashing_request;
    ] )

let tests_buffer =
  ("Buffer handling", [tztest "Dropbox/Async (eio handlers)" test_async_dropbox])

let () =
  Alcotest.run
    ~__FILE__
    "Bees_workers (eio handlers)"
    [tests_history; tests_status; tests_buffer]
