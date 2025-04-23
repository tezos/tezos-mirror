(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com       *)
(*                                                                           *)
(*****************************************************************************)

module Name = struct
  let base = ["task"]

  type t = string

  let encoding = Data_encoding.string

  let pp = Format.pp_print_string

  let equal = ( = )
end

type ('param, 'result) task = {
  name : string;
  on_completion : ('result -> unit) option;
  on_request : 'param -> 'result;
  param : 'param;
}

module Request = struct
  type ('a, 'b) t = Task : ('param, 'result) task -> ('result, 'error) t

  type view = string

  let encoding = Data_encoding.string

  let pp = Format.pp_print_string

  let view = function Task {name; _} -> "task(" ^ name ^ ")"
end

module Types = struct
  type parameters = unit

  type state = unit
end

module Worker = Worker.MakeSingle (Name) (Request) (Types)

type task_worker = Worker.infinite Worker.queue Worker.t

module Handlers : Worker.EIO_HANDLERS with type self = task_worker = struct
  type self = task_worker

  let on_request :
      self -> ('r, 'request_error) Request.t -> ('r, 'request_error) result =
   fun _ request ->
    match request with Task {on_request; param; _} -> Ok (on_request param)

  type launch_error

  let on_launch _ _ _ = Ok ()

  (* Return [Ok ()] in any case so that the worker does not crash if a request
     fails. *)
  let on_error _ _ _ _ = Ok ()

  let on_completion _ req res _ =
    match req with Request.Task {on_completion = Some f; _} -> f res | _ -> ()

  let on_no_request _ = ()

  let on_close _ = ()
end

type launch_error = Handlers.launch_error

type 'a message_error = 'a Worker.message_error =
  | Closed of error list option
  | Request_error of 'a
  | Any of exn

let worker name domains =
  let table = Worker.create_table Queue in
  Worker.launch_eio ~domains table ~name () (module Handlers)

let launch_task_and_wait worker name on_request ?on_completion param =
  let r = Request.Task {name; on_request; param; on_completion} in
  Worker.Queue.push_request_and_wait_eio worker r |> Lwt_eio.Promise.await_eio

let launch_tasks_and_wait worker name func ?on_completion args =
  Lwt_list.map_p (launch_task_and_wait worker name ?on_completion func) args

let launch_task worker name on_request ?on_completion param =
  let r = Request.Task {name; on_request; param; on_completion} in
  Worker.Queue.push_request_eio worker r
