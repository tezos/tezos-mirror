(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type worker =
  | Worker : {
      worker : _;
      launched : Time.System.t;
      switch : Eio.Switch.t;
      subdomains : int;
    }
      -> worker

module WorkerTbl = struct
  module Htbl = Saturn.Htbl

  type t = (string, worker) Htbl.t

  let create ~initial_size =
    let min_buckets = max 1 initial_size in
    Htbl.create ~hashed_type:(module String) ~min_buckets ()

  let find_opt tbl ~name = Htbl.find_opt tbl name

  let replace tbl ~name worker =
    if Htbl.try_add tbl name worker then ()
    else if Htbl.try_set tbl name worker then ()
    else (* unreachable *)
      assert false
end

type t = {
  workers : WorkerTbl.t;
  lwt_tasks_stream : (unit -> unit Lwt.t) Eio.Stream.t;
}

let hive =
  {
    workers = WorkerTbl.create ~initial_size:64;
    lwt_tasks_stream = Eio.Stream.create max_int;
  }

let async_lwt = Eio.Stream.add hive.lwt_tasks_stream

(* Initialize the [lwt_scheduler_loop] by running it in its own domain in the
   main Eio switch *)
let () =
  let lwt_scheduler_loop () =
    let rec loop () : [`Stop_daemon] =
      let lwt_closure = Eio.Stream.take hive.lwt_tasks_stream in
      (* The loop will run in the [Event_loop] main domain, so [Eio.run_lwt] is
         fine. *)
      Lwt_eio.run_lwt lwt_closure ;
      loop ()
    in
    loop ()
  in
  Tezos_base_unix.Event_loop.on_main_run (fun _env switch ->
      Eio.Fiber.fork_daemon ~sw:switch lwt_scheduler_loop)

exception Unknown_worker of string

let launch_worker (type worker) ?switch (worker : worker) ~bee_name ~domains
    worker_loop =
  let {workers; _} = hive in
  let env = Tezos_base_unix.Event_loop.env_exn () in

  (* The fibers created by [fork_daemon] will be cancelled by the switch used
     once the switch has finished processing all non-daemon fibers.

     If you create a switch here with [Switch.run] the daemon will be created
     and the switch will return, immediately freeing resources attached and
     making the worker inoperative.

     From the Eio.Switch documentation:
     > Any function creating resources that outlive [the switch] needs to be
     > given a switch by its caller.

     The Event_loop's main switch is meant to be a switch available during
     the whole life of your main process, so you can use it by default.
     This accessor must be called from the main domain; callers off-main should
     schedule via [run_on_main] to avoid blocking on the main switch lookup.
  *)
  let switch =
    match switch with
    | Some switch -> switch
    | None -> Tezos_base_unix.Event_loop.main_switch_exn ()
  in
  for i = 0 to domains - 1 do
    Eio.Fiber.fork_daemon ~sw:switch (fun () ->
        Eio.Domain_manager.run env#domain_mgr (fun () -> worker_loop i worker))
  done ;
  WorkerTbl.replace
    workers
    ~name:bee_name
    (Worker
       {worker; launched = Time.System.now (); subdomains = domains; switch})

let get_error bee_name =
  let {workers; _} = hive in
  match WorkerTbl.find_opt workers ~name:bee_name with
  (* Note that this branch shouldn't be reachable, as this function is supposed
     to be called from within an existing worker loop. *)
  | None -> raise (Unknown_worker bee_name)
  | Some (Worker {switch; _}) -> Eio.Switch.get_error switch

(* [main_job] encapsulates a closure to be executed on the main domain
   and a resolver to return the result to the caller. *)
type main_job =
  | Job : {
      run : unit -> 'a;
      resolver : ('a, exn) result Eio.Promise.u;
    }
      -> main_job

(* A stream of jobs to be executed on the main domain. *)
let main_jobs = Eio.Stream.create max_int

(* [run_main_jobs] is a daemon that consumes jobs from [main_jobs] and
   executes them. It is started on the main domain by [Event_loop.on_main_run]. *)
let () =
  let rec run_main_jobs () : [`Stop_daemon] =
    let (Job {run; resolver}) = Eio.Stream.take main_jobs in
    let outcome =
      match run () with value -> Ok value | exception exn -> Error exn
    in
    Eio.Promise.resolve resolver outcome ;
    run_main_jobs ()
  in
  Tezos_base_unix.Event_loop.on_main_run (fun _env switch ->
      Eio.Fiber.fork_daemon ~sw:switch run_main_jobs)

let run_on_main f =
  match Tezos_base_unix.Event_loop.main_switch () with
  | None ->
      invalid_arg
        "Tezos_bees.Hive.run_on_main called before Event_loop main_run \
         has          started"
  | Some _ -> (
      let promise, resolver = Eio.Promise.create () in
      Eio.Stream.add main_jobs (Job {run = f; resolver}) ;
      match Eio.Promise.await promise with
      | Ok value -> value
      | Error exn -> raise exn)
