(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let instance = ref None

exception Not_initialized

let env () = !instance

let env_exn () =
  match env () with None -> raise Not_initialized | Some env -> env

let init_eio_loop ~env () =
  (* Having [!instance <> None] should only happen if [main_run] is
     called within [main_run]. It will be caught up by [Eio_posix.main_run]
     but we can [assert false] just in case the error is not caught
     for some reason. *)
  assert (!instance = None) ;
  instance := Some env

let main_run ?(eio = false) promise =
  if eio then (
    let debug = Sys.getenv_opt "LWT_EIO_DEBUG" <> None in
    Eio_posix.run @@ fun env ->
    Lwt_eio.with_event_loop ~debug ~clock:env#clock @@ fun () ->
    init_eio_loop ~env () ;
    let res = Lwt_eio.run_lwt promise in
    (* While it shouldn't be necessary, there might be cases where
       Event_loop.main_run will be called consecutively as

       ```
       let () = Event_loop.main_run (* some initialization code *)

       let () = Event_loop.main_run (* main promise *)
       ```

       If the instance is not reset, the second Event_loop will use the Eio
       internal event loop from the first one, through the domain_mgr.
       Forcing it to be [None] should avoid this issue. *)
    instance := None ;
    res)
  else Lwt_main.run @@ promise ()

let main_run_eio promise =
  Eio_posix.run @@ fun env ->
  init_eio_loop ~env () ;
  let res = promise env in
  instance := None ;
  res
