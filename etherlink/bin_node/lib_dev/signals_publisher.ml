(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type parameters = unit

module Types = struct
  type nonrec parameters = parameters

  type state = unit

  let of_parameters () = ()
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = Signals_publisher_events.section

  let pp _ _ = ()

  let equal () () = true
end

module Request = struct
  type ('a, 'b) t = Dummy : (unit, tztrace) t

  type view = View : _ t -> view

  let view (req : _ t) = View req

  let encoding =
    let open Data_encoding in
    union
      [
        case
          (Tag 0)
          ~title:"Dummy"
          empty
          (function View Dummy -> Some ())
          (fun () -> View Dummy);
      ]

  let pp _ppf (View _) = ()
end

module Worker = struct
  include Worker.MakeSingle (Name) (Request) (Types)
end

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  type self = worker

  let on_request :
      type r request_error.
      worker -> (r, request_error) Request.t -> (r, request_error) result Lwt.t
      =
   fun _w request ->
    match request with
    | Dummy -> protect @@ fun () -> Lwt_result_syntax.return_unit

  type launch_error = error trace

  let on_launch _w () (parameters : Types.parameters) =
    Lwt_result_syntax.return (Types.of_parameters parameters)

  let on_error (type a b) _w _st (_r : (a, b) Request.t) (_errs : b) :
      unit tzresult Lwt.t =
    Lwt_result_syntax.return_unit

  let on_completion _ _ _ _ = Lwt.return_unit

  let on_no_request _ = Lwt.return_unit

  let on_close _ = Lwt.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_signals_publisher

let worker =
  let open Result_syntax in
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> return worker
    | Lwt.Fail e -> tzfail (error_of_exn e)
    | Lwt.Sleep -> tzfail No_signals_publisher)

let bind_worker f =
  let open Lwt_result_syntax in
  let res = Lazy.force worker in
  match res with
  | Error [No_signals_publisher] ->
      (* There is no worker, nothing to do *)
      return_unit
  | Error errs -> fail errs
  | Ok w -> f w

let worker_add_request ~request =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! (_pushed : bool) = Worker.Queue.push_request w request in
  return_unit

let start () =
  let open Lwt_result_syntax in
  let parameters = () in
  let* worker = Worker.launch table () parameters (module Handlers) in
  let*! () = Signals_publisher_events.publisher_is_ready () in
  Lwt.wakeup worker_waker worker ;
  return_unit

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Signals_publisher_events.publisher_shutdown () in
  let*! () = Worker.shutdown w in
  return_unit
