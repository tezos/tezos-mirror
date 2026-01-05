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
module Events = Task_worker_events

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
   overloading it. *)
let default_max_domains = max (min (Domain.recommended_domain_count () / 2) 8) 1

let number_of_domains = default_max_domains

let desired_domains = ref default_max_domains

let current_domains = ref default_max_domains

let double_resolution_logged = Atomic.make false

let log_double_resolution_once msg =
  if not (Atomic.get double_resolution_logged) then
    if Atomic.compare_and_set double_resolution_logged false true then
      Hive.async_lwt @@ fun () ->
      Events.(
        emit
          promise_double_resolution_detected
          (msg, Printexc.raw_backtrace_to_string (Printexc.get_callstack 16)))

let rec is_worker_launch_resource_error = function
  | Unix.Unix_error (Unix.ENOMEM, _, _) -> true
  | Invalid_argument msg
  (* Eio domain launch can sporadically surface an [Invalid_argument] about
       an already-resolved promise when domain bootstrapping races; treat that
       as a transient resource-style failure that can succeed on retry. *)
    when String.starts_with ~prefix:"Can't resolve already-resolved promise" msg
         || String.starts_with ~prefix:"Double resolve of a promise" msg ->
      log_double_resolution_once msg ;
      true
  | Eio.Exn.Multiple trace ->
      List.exists (fun (exn, _) -> is_worker_launch_resource_error exn) trace
  | _ -> false

(* [launch_worker_with_domains] tries to launch with [domains] and, on transient
   resource errors, retries with [domains/2] until it succeeds or reaches 1. *)
let rec launch_worker_with_domains table domains =
  match
    try
      Ok
        (Worker.launch_eio
           ~domains
           table
           ~name:"shared task worker"
           ()
           (module Handlers))
    with exn -> Error exn
  with
  | Ok (Ok w) ->
      current_domains := domains ;
      if domains <> !desired_domains then
        Hive.async_lwt (fun () ->
            Events.(emit worker_launch_degraded (!desired_domains, domains))) ;
      w
  | Ok (Error _) -> assert false
  | Error exn ->
      Hive.async_lwt (fun () ->
          Events.(emit worker_launch_failed (Printexc.to_string exn, domains))) ;
      if domains > 1 && is_worker_launch_resource_error exn then (
        let next_domains = max 1 (domains / 2) in
        Hive.async_lwt (fun () -> Events.(emit worker_retrying next_domains)) ;
        launch_worker_with_domains table next_domains)
      else raise exn

let make_worker () =
  let table = Worker.create_table Queue in
  Eio.Lazy.from_fun ~cancel:`Protect @@ fun () ->
  launch_worker_with_domains table !desired_domains

let worker_lock = Eio.Mutex.create ()

let worker = ref (make_worker ())

let with_worker_state f =
  Eio.Mutex.lock worker_lock ;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock worker_lock) f

(* [get_worker] returns a worker instance, creating it if it doesn't exist.
   Access is protected by a mutex to prevent race conditions from multiple
   domains. *)
let get_worker () =
  with_worker_state (fun () ->
      try Eio.Lazy.force !worker
      with exn ->
        (* If forcing fails, reset the lazy so that the next call will try
           to recreate the worker.  *)
        worker := make_worker () ;
        raise exn)

let shutdown () =
  with_worker_state (fun () ->
      try
        let w = Eio.Lazy.force !worker in
        Hive.async_lwt (fun () -> Events.(emit shutdown_enter ())) ;
        Fun.protect
          ~finally:(fun () ->
            Hive.async_lwt (fun () -> Events.(emit shutdown_exit ())))
          (fun () ->
            Worker.shutdown_eio w ;
            worker := make_worker ())
      with _ ->
        (* If forcing fails or wasn't built yet, just reset the lazy. *)
        worker := make_worker ())

let launch_task_and_wait name on_request ?on_completion param =
  let r = Request.Task {name; on_request; param; on_completion} in
  Worker.Queue.push_request_and_wait_eio (get_worker ()) r

let launch_tasks_and_wait ?(max_fibers = max_int) name func ?on_completion args
    =
  Eio.Fiber.List.map
    ~max_fibers
    (fun arg ->
      launch_task_and_wait name ?on_completion func arg |> Eio.Promise.await)
    args

exception Bee_task_worker_error of string

let bind_and_raise p =
  let error_wrapper = Format.sprintf "Task worker error: %s" in
  match p with
  | Ok v -> v
  | Error (Closed (Some err)) ->
      raise
        (Bee_task_worker_error
           (error_wrapper (Format.asprintf "%a" Error_monad.pp_print_trace err)))
  | Error (Closed None) ->
      raise (Bee_task_worker_error (error_wrapper "closed error"))
  | Error (Request_error _) ->
      raise (Bee_task_worker_error (error_wrapper "request error"))
  | Error (Any exn) ->
      raise (Bee_task_worker_error (error_wrapper (Printexc.to_string exn)))

let bind_and_raise_all l = List.map bind_and_raise l

let launch_task name on_request ?on_completion param =
  let r = Request.Task {name; on_request; param; on_completion} in
  Worker.Queue.push_request_eio (get_worker ()) r
