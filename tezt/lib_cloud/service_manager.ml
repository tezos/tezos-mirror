(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)
let section = "Service_manager"

type service = {
  mutable executable : string option;
  on_alive_callback : alive:bool -> unit;
  mutable pid : int option;
  mutable on_shutdown : (unit -> unit Lwt.t) list;
}

type t = {
  mutable services : (string, service) Hashtbl.t;
  worker_promise : unit Lwt.t;
  worker_waker : unit Lwt.u;
}

let init () =
  let worker_promise, worker_waker = Lwt.task () in
  {services = Hashtbl.create 10; worker_promise; worker_waker}

let run ~name service =
  match service.pid with
  | None ->
      Log.info "%s: service is not (yet) running %s" section name ;
      service.on_alive_callback ~alive:false ;
      (* Service is not 'declared' as running, no need to alert *)
      Lwt.return_unit
  | Some pid -> (
      let fn = Format.asprintf "/proc/%d/exe" pid in
      match Unix.readlink fn with
      | fn -> (
          match service.executable with
          | None ->
              (* Allow setting the executable once, in case it was incorrectly
                 setup at registration (e.g. in docker) *)
              service.executable <- Some fn ;
              service.pid <- Some pid ;
              service.on_alive_callback ~alive:true ;
              Lwt.return_unit
          | Some fn' ->
              (* The pid might have been recycled.
                 Checks the value is equal to the executable name provided to
                 ensure this is not the case.
                 Note: Unix.realpath should not return an error, because readlink
                 always returns an existing executable *)
              let fn = Unix.realpath fn in
              if fn <> fn' then (
                Log.error
                  "%s: service %s, identified by pid %d is not the expected \
                   executable. Probable crash (replaced by %s)"
                  section
                  fn
                  pid
                  fn' ;
                (* log the error only once, then do not check again *)
                let () = service.pid <- None in
                let () = service.on_alive_callback ~alive:false in
                Lwt.return_unit)
              else
                let () = service.on_alive_callback ~alive:true in
                Lwt.return_unit)
      | exception Unix.(Unix_error (ENOENT, _, _)) ->
          (* The monitored pid is not running anymore *)
          Log.error
            "%s: service %s, identifier by pid %d is not running anymore."
            section
            name
            pid ;
          service.on_alive_callback ~alive:false ;
          (* log the error only once, then do not check again *)
          service.pid <- None ;
          Lwt.return_unit
      | exception exn ->
          Test.fail "Error in service manager: %s" (Printexc.to_string exn))

let start t =
  let rec loop () =
    let* () = Lwt_unix.sleep 1. in
    let* () =
      Lwt_seq.iter_s
        (fun (name, service) -> run ~name service)
        (Hashtbl.to_seq t.services |> Lwt_seq.of_seq)
    in
    loop ()
  in
  Background.register (Lwt.pick [loop (); t.worker_promise])

let register_service ~name ~executable
    ?(on_alive_callback =
      fun ~alive ->
        ignore alive ;
        ()) ~on_shutdown t =
  (* Start only when needed *)
  let () = if Hashtbl.length t.services = 0 then start t else () in
  (* Get the real executable name *)
  (* Note: this only works on remote vm *)
  let service =
    let executable =
      if Sys.file_exists executable then Some (Unix.realpath executable)
      else None
    in
    {executable; on_alive_callback; pid = None; on_shutdown}
  in
  let () = Hashtbl.add t.services name service in
  Log.info "%s: Registering service: %s (%s)" section name executable

let notify_start_service ~name ~pid t =
  match Hashtbl.find_opt t.services name with
  | None ->
      Log.warn
        "%s: Cannot find service identified by %s. Not registered"
        section
        name
  | Some service ->
      let () =
        Log.info "%s: Notify start service %s (pid %d)" section name pid
      in
      service.on_alive_callback ~alive:true ;
      service.pid <- Some pid

let notify_stop_service ~name t =
  match Hashtbl.find_opt t.services name with
  | None ->
      Log.warn
        "%s: Cannot find service identified by %s. Not registered"
        section
        name
  | Some service ->
      let () = Log.info "%s: Notify stop service %s" section name in
      service.pid <- None

let shutdown t =
  let on_shutdown_callbacks =
    Hashtbl.fold
      (fun name service acc -> (name, service.on_shutdown) :: acc)
      t.services
      []
  in
  let* () =
    Lwt_list.iter_s
      (fun (name, callbacks) ->
        Log.info
          "Running service manager shutdown callback for service: %s"
          name ;
        Lwt_list.iter_s (fun callback -> callback ()) callbacks)
      on_shutdown_callbacks
  in
  Lwt.wakeup t.worker_waker () ;
  Lwt.return_unit
