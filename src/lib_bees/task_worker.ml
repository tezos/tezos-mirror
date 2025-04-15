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

module Request = struct
  type ('a, 'b) t =
    | Task : (string * ('param -> 'result) * 'param) -> ('result, 'error) t

  type view = string

  let encoding = Data_encoding.string

  let pp = Format.pp_print_string

  let view = function Task (name, _, _) -> "task(" ^ name ^ ")"
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
   fun _ request -> match request with Task (_, f, param) -> Ok (f param)

  type launch_error

  let on_launch _ _ _ = Ok ()

  (* Return [Ok ()] in any case so that the worker does not crash if a request
     fails. *)
  let on_error _ _ _ _ = Ok ()

  let on_completion _ _ _ _ = ()

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

let launch_task worker name func arg =
  let r = Request.Task (name, func, arg) in
  Worker.Queue.push_request_and_wait_eio worker r |> Lwt_eio.Promise.await_eio

let launch_tasks worker name func args =
  Lwt_list.map_p (launch_task worker name func) args
