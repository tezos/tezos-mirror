(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022-2025 Nomadic Labs. <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_bees workers
    Invocation:   dune exec src/lib_bees/test/main.exe \
                  -- --file test_bees_unit_eio.ml
    Subject:      Unit tests for [Lib_bees]
*)

module Assert = Assert
open Mocked_worker

exception RaisedExn

let sleep d =
  let env = Tezos_base_unix.Event_loop.env_exn () in
  Eio.Time.sleep env#clock d

let emit_event =
  let emit_event = emit_event "test_bees_unit_eio" in
  fun req -> Tezos_bees.Hive.async_lwt (fun () -> emit_event req)

let create_handlers (type a) ?on_completion ?on_close ?(slow = false) () =
  (module struct
    type self = a Worker.t

    let on_request : type r request_error.
        self -> (r, request_error) Request.t -> (r, request_error) result =
     fun _w request ->
      let () = if slow then sleep 0.2 else () in
      let () = emit_event (Request.view request) in
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

    let on_close _w = match on_close with Some f -> f () | None -> ()
  end : Worker.EIO_HANDLERS
    with type self = a Worker.t
     and type launch_error = error trace)

let create table handlers ?(timeout : float option) name =
  let timeout =
    Option.bind timeout (fun timeout ->
        Ptime.(of_float_s timeout |> Option.map Ptime.to_span))
  in
  Worker.launch_eio ?timeout table ~name 0 handlers

let create_queue ?timeout ?on_completion ?on_close =
  let table = Worker.create_table Queue in
  create ?timeout table (create_handlers ?on_completion ?on_close ())

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
        match create_queue "random_worker" with
        | Ok w ->
            let r =
              Fun.protect
                ~finally:(fun () -> Worker.shutdown_eio w)
                (fun () -> push_multiple_requests w l)
            in
            assert_status w "Closed" ;
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
  let t, u = Eio.Promise.create () in
  let* w =
    create_queue
      ~on_close:(fun () -> Eio.Promise.resolve u ())
      "crashing_worker"
  in
  assert_status w "Running" ;
  push_and_assert_history w [Box RqB; Box RqB; Box (RqErr Crash); Box RqB] ;
  let () = Eio.Promise.await t in
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
  Fun.protect
    ~finally:(fun () ->
      Worker.shutdown_eio w ;
      assert_status w "Closed")
    (fun () ->
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
      return_unit)

let wrap_qcheck test () =
  let _ = QCheck_alcotest.to_alcotest test in
  Result_syntax.return_unit

let test_worker_contention () =
  let open Result_syntax in
  (* Scenario: hammer the shared task worker from many fibers at once to ensure
     contention doesn't break task execution or worker liveness. *)
  let parallel_fibers =
    min 16 (max 4 (Tezos_bees.Task_worker.number_of_domains * 2))
  in
  let run_job payload =
    Tezos_bees.Task_worker.launch_task_and_wait
      "contention"
      (fun id ->
        for _ = 1 to 200 do
          ignore (Sys.opaque_identity ())
        done ;
        id)
      payload
    |> Eio.Promise.await
  in
  let total_jobs = parallel_fibers * 4 in
  let inputs =
    let rec aux acc i =
      if i = total_jobs then List.rev acc else aux (i :: acc) (i + 1)
    in
    aux [] 0
  in
  Eio.Fiber.List.iter
    ~max_fibers:parallel_fibers
    (fun payload ->
      match run_job payload with
      | Ok _ -> ()
      | Error (Closed _) ->
          Alcotest.fail "worker unexpectedly closed while handling contention"
      | Error (Request_error _) ->
          Alcotest.fail "worker request error under contention"
      | Error (Any exn) -> raise exn)
    inputs ;
  return_unit

let test_worker_launch_race () =
  let open Result_syntax in
  (* Scenario: spin up multiple domains racing to launch tasks at startup,
     exercising worker creation path under simultaneous requests. *)
  let describe_message_error : _ Tezos_bees.Task_worker.message_error -> string
      = function
    | Closed _ -> "Closed"
    | Request_error _ -> "Request_error"
    | Any exn -> Format.asprintf "Any(%s)" (Printexc.to_string exn)
  in
  let fail_on_error err =
    let msg = describe_message_error err in
    Alcotest.failf "task worker failure: %s" msg
  in
  let env = Tezos_base_unix.Event_loop.env_exn () in
  let concurrent_domains =
    max 4 (Tezos_bees.Task_worker.number_of_domains * 4)
  in
  let requests_per_domain = 6 in
  let iterations = 5 in
  let run_job () =
    match
      Tezos_bees.Task_worker.launch_task_and_wait
        "task-worker-race"
        (fun () -> ())
        ()
      |> Eio.Promise.await
    with
    | Ok () -> ()
    | Error err -> fail_on_error err
  in
  let wait_for readiness target =
    let rec loop () =
      if Atomic.get readiness < target then (
        Eio.Time.sleep env#clock 0.00005 ;
        loop ())
    in
    loop ()
  in
  let run_iteration iteration =
    let readiness = Atomic.make 0 in
    let start_wait, start_signal = Eio.Promise.create () in
    let main_switch = Tezos_base_unix.Event_loop.main_switch_exn () in
    let domain_tasks =
      Array.init concurrent_domains (fun _domain_id ->
          Eio.Fiber.fork_promise ~sw:main_switch (fun () ->
              Eio.Domain_manager.run env#domain_mgr (fun () ->
                  ignore (Atomic.fetch_and_add readiness 1) ;
                  Eio.Promise.await start_wait ;
                  for _ = 1 to requests_per_domain do
                    run_job ()
                  done)))
    in
    wait_for readiness concurrent_domains ;
    Eio.Promise.resolve start_signal () ;
    Array.iter
      (fun promise ->
        match Eio.Promise.await promise with
        | Ok () -> ()
        | Error exn -> raise exn)
      domain_tasks ;
    Format.printf "[race] iteration %d/%d complete@\n%!" iteration iterations
  in
  Fun.protect ~finally:Tezos_bees.Task_worker.shutdown (fun () ->
      for iteration = 1 to iterations do
        run_iteration iteration
      done) ;
  return_unit

(** Tests run in fresh processes via [Alcotezt_process] so we do not fork after
    Eio/domains have been initialised within the current Tezt worker. *)
let tztest ?timeout label fn =
  Alcotezt_process.test_case ?timeout label `Quick fn

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

let tests_contention =
  ( "Task worker contention",
    [
      tztest
        ~timeout:30.
        "Worker creation under contention (eio handlers)"
        test_worker_contention;
      tztest
        ~timeout:30.
        "Worker launch race across domains (eio handlers)"
        test_worker_launch_race;
    ] )

let () =
  Alcotezt_process.run
    ~__FILE__
    "Bees_workers (eio handlers)"
    [tests_history; tests_status; tests_buffer; tests_contention]
