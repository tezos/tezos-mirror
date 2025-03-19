(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let section = "Service_manager"

type service = {executable : string; mutable pid : int option}

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
      (* Service is not 'declared' as running, no need to alert *)
      Lwt.return_unit
  | Some pid -> (
      let fn = Format.asprintf "/proc/%d/exe" pid in
      match Unix.readlink fn with
      | fn ->
          (* The pid could have been recycled.
             Checks the value is equal to the executable name provided to
             ensure this is not the case.
             Note: Unix.realpath should not return an error, because readlink
             always returns an existing executable *)
          let fn = Unix.realpath fn in
          if fn <> service.executable then
            let () =
              Log.error
                "%s: service %s, identified by pid %d is not the expected \
                 executable. Probable crash (replaced by %s)"
                section
                service.executable
                pid
                fn
            in
            (* log the error only once, then do not check again *)
            let () = service.pid <- None in
            Lwt.return_unit
          else Lwt.return_unit
      | exception Unix.(Unix_error (ENOENT, _, _)) ->
          (* The monitored pid is not running anymore *)
          Log.error
            "%s: service %s, identifier by pid %d is not running anymore."
            section
            name
            pid ;
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

let register_service ~name ~executable t =
  (* Start only when needed *)
  let () = if Hashtbl.length t.services = 0 then start t else () in
  (* Get the real executable name *)
  if Sys.file_exists executable then
    let executable = Unix.realpath executable in
    let service = {executable; pid = None} in
    let () = Hashtbl.add t.services name service in
    Log.info "%s: Registering service: %s (%s)" section name executable
  else Log.error "%s: Cannot find executable %s on current system" section name

let notify_start_service ~name ~pid t =
  match Hashtbl.find_opt t.services name with
  | None ->
      Log.warn
        "%s: Cannot find service identified by %s. Not registered"
        section
        name
  | Some service ->
      let () =
        Log.info "%s Notify start service %s (pid %d)" section name pid
      in
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

let shutdown t = Lwt.wakeup t.worker_waker ()
