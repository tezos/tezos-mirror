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
  type t = worker String.Hashtbl.t

  let create ~initial_size = String.Hashtbl.create initial_size

  let find_opt tbl ~name = String.Hashtbl.find_opt tbl name
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

let async_lwt = Eio.Stream.add hive.lwt_tasks_stream

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
  String.Hashtbl.add
    workers
    bee_name
    (Worker
       {worker; launched = Time.System.now (); subdomains = domains; switch})

let get_error bee_name =
  let {workers; _} = hive in
  match WorkerTbl.find_opt workers ~name:bee_name with
  (* Note that this branch shouldn't be reachable, as this function is supposed
     to be called from within an existing worker loop. *)
  | None -> raise (Unknown_worker bee_name)
  | Some (Worker {switch; _}) -> Eio.Switch.get_error switch
