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

type t = {workers : WorkerTbl.t}

let hive = {workers = WorkerTbl.create ~initial_size:64}

exception Unknown_worker of string

let launch_worker (type worker) (worker : worker) ~bee_name ~domains worker_loop
    =
  let {workers} = hive in
  let env = Tezos_base_unix.Event_loop.env_exn () in
  let switch = Tezos_base_unix.Event_loop.main_switch_exn () in
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
  let {workers} = hive in
  match WorkerTbl.find_opt workers ~name:bee_name with
  (* Note that this branch shouldn't be reachable, as this function is supposed
     to be called from within an existing worker loop. *)
  | None -> raise (Unknown_worker bee_name)
  | Some (Worker {switch; _}) -> Eio.Switch.get_error switch
