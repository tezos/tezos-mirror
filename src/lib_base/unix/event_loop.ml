(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = {env : Eio_unix.Stdenv.base; main_switch : Eio.Switch.t}

let instance = ref None

exception Not_initialized

let env () = Option.map (fun {env; _} -> env) !instance

let env_exn () =
  match env () with None -> raise Not_initialized | Some env -> env

let main_switch () = Option.map (fun {main_switch; _} -> main_switch) !instance

let main_switch_exn () =
  match main_switch () with
  | None -> raise Not_initialized
  | Some main_switch -> main_switch

let on_main_run_callbacks = ref []

let on_main_run callback =
  on_main_run_callbacks := !on_main_run_callbacks @ [callback]

let init_eio_loop ~env ~switch () =
  (* Having [!instance <> None] should only happen if [main_run] is
     called within [main_run]. It will be caught up by [eio_posix.main_run]
     but we can [assert false] just in case the error is not caught
     for some reason. *)
  assert (!instance = None) ;
  instance := Some {env; main_switch = switch} ;
  List.iter (fun callback -> callback env switch) !on_main_run_callbacks

let main_run ?(eio = false) ~process_name promise =
  Gc_setup.set_gc_space_overhead process_name ;
  if eio then (
    let debug = Sys.getenv_opt "LWT_EIO_DEBUG" <> None in
    Eio_posix.run @@ fun env ->
    Lwt_eio.with_event_loop ~debug ~clock:env#clock @@ fun () ->
    Eio.Switch.run @@ fun switch ->
    init_eio_loop ~env ~switch () ;
    let res = Lwt_eio.run_lwt promise in
    (* While it shouldn't be necessary, there might be cases where
       Event_loop.main_run will be called consecutively as

       ```
       let () = Event_loop.main_run (* some initialization code *)

       let () = Event_loop.main_run (* main promise *)
       ```

       If the instance is not reset, the second Event_loop will use the main
       switch from the first one, which is relevant at this moment of the code.
       Forcing it to be [None] should avoid this issue, and also avoid some
       value never being garbage collected. *)
    instance := None ;
    res)
  else Lwt_main.run @@ promise ()

let main_run_eio promise =
  Eio_posix.run @@ fun env ->
  Eio.Switch.run @@ fun switch ->
  init_eio_loop ~env ~switch () ;
  let res = promise env in
  instance := None ;
  res
