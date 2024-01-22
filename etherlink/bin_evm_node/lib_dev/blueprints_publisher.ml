(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type config = {rollup_node_endpoint : Uri.t}

type state = {config : config; mutable degraded : bool}

module Types = struct
  type nonrec state = state

  type parameters = config
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = Blueprint_event.section

  let pp _fmt () = ()

  let equal () () = true
end

module Worker = struct
  include Worker.MakeSingle (Name) (Blueprints_publisher_types.Request) (Types)

  let is_degraded worker = (state worker).degraded

  let rollup_node_endpoint worker = (state worker).config.rollup_node_endpoint

  let enters_degraded_mode worker = (state worker).degraded <- true
end

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  open Blueprints_publisher_types

  type self = worker

  type launch_error = error trace

  let on_launch _self () config = Lwt_result.return {degraded = false; config}

  let on_request :
      type r request_error.
      self -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
   fun self request ->
    let open Lwt_syntax in
    match request with
    | Publish {level; payload} when not (Worker.is_degraded self) -> (
        let rollup_node_endpoint = Worker.rollup_node_endpoint self in
        let* res = Rollup_node_services.publish ~rollup_node_endpoint payload in
        match res with
        | Ok () ->
            let* () = Blueprint_event.blueprint_injected level in
            Lwt_result_syntax.return_unit
        | Error _ ->
            Worker.enters_degraded_mode self ;
            let* () = Blueprint_event.entered_degraded_mode level in
            Lwt_result_syntax.return_unit)
    | Publish _ ->
        (* Degraded mode: nothing to do *)
        Lwt_result_syntax.return_unit

  let on_completion (type a err) _self (_r : (a, err) Request.t) (_res : a) _st
      =
    Lwt_syntax.return_unit

  let on_no_request _self = Lwt.return_unit

  let on_close _self = Lwt.return_unit

  let on_error (type a b) _self _st (_r : (a, b) Request.t) (_errs : b) :
      unit tzresult Lwt.t =
    Lwt_result_syntax.return_unit
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

let start node_ctxt =
  let open Lwt_result_syntax in
  let rollup_node_endpoint = node_ctxt.rollup_node_endpoint in
  let* worker =
    Worker.launch table () {rollup_node_endpoint} (module Handlers)
  in
  let*! () = Blueprint_event.publisher_is_ready () in
  Lwt.wakeup worker_waker worker ;
  return_unit

type error += No_worker

let worker =
  let open Result_syntax in
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> return worker
    | Lwt.Fail exn -> fail (Error_monad.error_of_exn exn)
    | Lwt.Sleep -> Error No_worker)

let worker_add_request ~request =
  let open Lwt_result_syntax in
  match Lazy.force worker with
  | Ok w ->
      let*! (_pushed : bool) = Worker.Queue.push_request w request in
      return_unit
  | Error No_worker -> return_unit
  | Error e -> tzfail e

let publish level payload =
  worker_add_request ~request:(Publish {level; payload})

let shutdown () =
  match Lazy.force worker with
  | Error _ ->
      (* There is no publisher, nothing to do *)
      Lwt.return_unit
  | Ok w -> Worker.shutdown w
