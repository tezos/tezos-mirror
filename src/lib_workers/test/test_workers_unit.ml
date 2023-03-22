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
    Invocation:   dune exec src/lib_workers/test/main.exe
    Subject:      Unit tests for [Worker]
*)

module Assert = Assert
open Mocked_worker
module Event = Internal_event.Simple

type error += TzCrashError

exception RaisedExn

let create_handlers (type a) ?on_completion () =
  (module struct
    type self = a Worker.t

    let on_request :
        type r request_error.
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

    type launch_error = error trace

    let on_launch _w _name _param =
      let open Lwt_result_syntax in
      return (ref [])

    let on_error (type a b) w _st (r : (a, b) Request.t) (errs : b) :
        unit tzresult Lwt.t =
      let open Lwt_result_syntax in
      let history = Worker.state w in
      match r with
      | Request.RqA _ -> return_unit
      | Request.RqB -> return_unit
      | Request.RqErr _ -> (
          match errs with
          | `CrashError -> Lwt.return_error [TzCrashError]
          | `SimpleError ->
              history := "RqErr" :: !history ;
              return_unit)

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
        match on_completion with Some f -> f () | None -> Lwt.return ()
      in
      Lwt.return_unit

    let on_no_request _ = Lwt.return_unit

    let on_close _w = Lwt.return_unit
  end : Worker.HANDLERS
    with type self = a Worker.t
     and type launch_error = error trace)

let create table handlers ?timeout name =
  Worker.launch ?timeout table name 0 handlers

let create_queue ?on_completion =
  let table = Worker.create_table Queue in
  create table (create_handlers ?on_completion ())

let create_bounded ?on_completion =
  let table = Worker.create_table (Bounded {size = 2}) in
  create table (create_handlers ?on_completion ())

let create_dropbox ?on_completion =
  let table =
    let open Worker in
    let merge _w (Any_request neu) (old : _ option) =
      let (Any_request r) =
        match (neu, old) with
        | RqA i1, Some (Any_request (RqA i2)) -> Any_request (RqA (i1 + i2))
        | (RqA _ as rqa), _ -> Any_request rqa
        | _, Some (Any_request (RqA _ as rqa)) -> Any_request rqa
        | RqB, _ -> Any_request neu
        | RqErr _, _ -> Any_request neu
      in
      Some (Worker.Any_request r)
    in
    Worker.create_table (Dropbox {merge})
  in
  create table (create_handlers ?on_completion ())

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

(* Checks a non crashing request actually doesn't make the worker crash *)
let test_push_crashing_request () =
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

(* On dropbox mode, checks asynchronous requests leads to merge when the
   completion takes time *)
let test_async_dropbox () =
  let open Lwt_result_syntax in
  let t_end, u_end = Lwt.task () in
  let t_each, u_each = Lwt.task () in
  let nb_completion = ref 0 in
  let* w =
    create_dropbox
      ~on_completion:(fun () ->
        let open Lwt_syntax in
        incr nb_completion ;
        let* () = t_each in
        if !nb_completion = 2 then Lwt.wakeup u_end () ;
        Lwt.return_unit)
      "dropbox_worker"
  in
  let rq = RqA 1 in
  let n = 10 in
  (* One blocking request is sent *)
  Worker.Dropbox.put_request w rq ;
  (* While the blocking request is handled, n other requests are sent *)
  (* There requests should be merged into one *)
  for _i = 1 to n do
    Worker.Dropbox.put_request w rq
  done ;
  Lwt.wakeup u_each () ;
  let*! () = t_end in
  (* Hence the expected result being two requests, the first blocking one *)
  (* and the second being the result of the merge *)
  let expected = build_expected_history [Box (RqA 1); Box (RqA n)] in
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
    ] )

let tests_buffer =
  ("Buffer handling", [Tztest.tztest "Dropbox/Async" `Quick test_async_dropbox])

let () =
  Alcotest_lwt.run "Workers" [tests_history; tests_status; tests_buffer]
  |> Lwt_main.run
