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

type 'a message_error = 'a Worker.message_error =
  | Closed of error list option
  | Request_error of 'a
  | Any of exn

(* This is a conservative limit that aims to fit all machines, without
   overloading it.*)
let default_max_domains = max (min (Domain.recommended_domain_count () / 2) 8) 1

let number_of_domains = default_max_domains

let worker =
  let table = Worker.create_table Queue in
  Eio.Lazy.from_fun ~cancel:`Protect @@ fun () ->
  match
    Worker.launch_eio
      ~domains:number_of_domains
      table
      ~name:"shared task worker"
      ()
      (module Handlers)
  with
  | Ok w -> w
  | Error _ -> assert false

let launch_task_and_wait name on_request ?on_completion param =
  let r = Request.Task {name; on_request; param; on_completion} in
  Worker.Queue.push_request_and_wait_eio (Eio.Lazy.force worker) r

let launch_tasks_and_wait ?(max_fibers = max_int) name func ?on_completion args
    =
  Eio.Fiber.List.map
    ~max_fibers
    (fun arg ->
      launch_task_and_wait name ?on_completion func arg |> Eio.Promise.await)
    args

let launch_task name on_request ?on_completion param =
  let r = Request.Task {name; on_request; param; on_completion} in
  Worker.Queue.push_request_eio (Eio.Lazy.force worker) r
