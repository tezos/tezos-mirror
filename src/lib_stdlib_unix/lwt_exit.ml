(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Lwt.Infix

(* 1. clean-up callback registration/unregistration *)

(* Identifiers are used for unregistering clean-up callbacks *)
type clean_up_callback_id = int

let clean_up_callback_id_counter = ref min_int

let new_clean_up_callback_id () =
  incr clean_up_callback_id_counter ;
  !clean_up_callback_id_counter

(* clean-up callbacks are stored in a reference to a map *)
module Callbacks_map = Map.Make (Int)

let clean_up_callbacks : (int -> unit Lwt.t) Callbacks_map.t ref =
  ref Callbacks_map.empty

(* adding and removing clean-up callbacks affects the global reference map *)
let register_clean_up_callback f =
  let id = new_clean_up_callback_id () in
  clean_up_callbacks := Callbacks_map.add id f !clean_up_callbacks ;
  id

let unregister_clean_up_callback id =
  clean_up_callbacks := Callbacks_map.remove id !clean_up_callbacks

(* 2. clean-up *)

(* cleaning-up is just calling all the clean-up callbacks, note that the
   function is not exported: it cannot be called directly, it can only be
   triggered as a side effect to calling [exit_and_raise] or [exit_and_wait] *)
let clean_up status =
  let callbacks = List.of_seq @@ Callbacks_map.to_seq !clean_up_callbacks in
  clean_up_callbacks := Callbacks_map.empty ;
  Lwt_list.iter_p
    (fun (_, c) ->
      Lwt.catch
        (fun () -> c status)
        (fun exc ->
          Format.eprintf
            "Exit: uncaught exception during clean-up: %s\n%!"
            (Printexc.to_string exc) ;
          Lwt.return_unit))
    callbacks

(* 3. synchronisation primitives *)

(* [clean_up_starts] an exported promise that resolves when the clean-up starts.
   [start_exiting] a non-exported resolver for the promise *)
let (clean_up_starts, start_clean_up) = Lwt.wait ()

(* [clean_up_ends] is a promise that resolves once the clean-up is finished. *)
let clean_up_ends =
  clean_up_starts
  >>= fun status -> clean_up status >>= fun () -> Lwt.return status

(* 4. exiting *)

(* simple exit is not exported, it is just to factor out exiting *)
let exit n =
  match Lwt.state clean_up_starts with
  | Sleep ->
      Lwt.wakeup start_clean_up n
  | Return _ ->
      ()
  | Fail _ ->
      assert false

(* [exit_and_raise] is meant to be used deep inside the program after having
   witnessed, say, a fatal error. It raises an exception so that it can be used
   anywhere in the program. *)
let exit_and_raise n = exit n ; raise Exit

(* [exit_and_wait] is meant to be used near the main invocation of the program,
   right inside of [Lwt_main.run] but presumably after [wrap_and_error]. *)
let exit_and_wait n = exit n ; clean_up_ends

(* 5. signals *)

(** Known signals and their names *)
let signals =
  let open Sys in
  [ (sigabrt, "ABRT");
    (sigalrm, "ALRM");
    (sigfpe, "FPE");
    (sighup, "HUP");
    (sigill, "ILL");
    (sigint, "INT");
    (sigkill, "KILL");
    (sigpipe, "PIPE");
    (sigquit, "QUIT");
    (sigsegv, "SEGV");
    (sigterm, "TERM");
    (sigusr1, "USR1");
    (sigusr2, "USR2");
    (sigchld, "CHLD");
    (sigcont, "CONT");
    (sigstop, "STOP");
    (sigtstp, "TSTP");
    (sigttin, "TTIN");
    (sigttou, "TTOU");
    (sigvtalrm, "VTALRM");
    (sigprof, "PROF");
    (sigbus, "BUS");
    (sigpoll, "POLL");
    (sigsys, "SYS");
    (sigtrap, "TRAP");
    (sigurg, "URG");
    (sigxcpu, "XCPU");
    (sigxfsz, "XFSZ") ]

(** recovering the name of a signal *)
let signal_name signal =
  match List.assoc_opt signal signals with
  | Some name ->
      name
  | None ->
      Format.asprintf "%d" signal

(* soft handling: trigger an exit on first signal, immediately terminate
   process on second signal *)
let set_soft_handler signal =
  let name = signal_name signal in
  let already_received_once = ref false in
  Lwt_unix.on_signal signal (fun _signal ->
      if !already_received_once then (
        Format.eprintf
          "%s: signal received again, forcing immediate termination.\n%!"
          name ;
        Stdlib.exit 1 )
      else
        match Lwt.state clean_up_starts with
        | Sleep ->
            Format.eprintf "%s: triggering shutdown.\n%!" name ;
            Format.eprintf "%s: send signal again to force-quit.\n%!" name ;
            already_received_once := true ;
            exit 1
        | Return _ ->
            Format.eprintf "%s: already in shutdown.\n%!" name ;
            Format.eprintf "%s: send signal again to force-quit.\n%!" name ;
            already_received_once := true
        | Fail _ ->
            assert false)

(* hard handling: immediately terminate process *)
let set_hard_handler signal =
  let name = signal_name signal in
  Lwt_unix.on_signal signal (fun _signal ->
      Format.eprintf "%s: force-quiting.\n%!" name ;
      Stdlib.exit 1)

let unset_handler = Lwt_unix.disable_signal_handler

(* 6. internal synchronisation *)

let sleep_span s = Lwt_unix.sleep (Ptime.Span.to_float_s s)

let wait_for_clean_up max_clean_up_time =
  (match Lwt.state clean_up_starts with Return _ -> () | _ -> assert false) ;
  match Lwt.state clean_up_ends with
  | Fail _ ->
      assert false
  | Return _ ->
      Lwt.pause ()
  | Sleep ->
      ( match max_clean_up_time with
      | None ->
          (* without timeout: just wait *)
          clean_up_ends >>= fun _ -> Lwt.return_unit
      | Some s ->
          (* with timeout: pick first to finish *)
          Lwt.pick [(clean_up_ends >>= fun _ -> Lwt.return_unit); sleep_span s]
      )
      (* pause in case timeout and clean-up needs to deal with cancellation *)
      >>= Lwt.pause

(* 7. main interface: wrapping promises *)

(* take a promise and wrap it in `Ok` but also watch for exiting and wrap that
   in `Error` *)
let wrap_and_error ?max_clean_up_time p =
  (* set signal handling (TODO: provide parameter to customise that) *)
  let int_handler = set_soft_handler Sys.sigint in
  let term_handler = set_hard_handler Sys.sigterm in
  Lwt.try_bind
    (fun () ->
      (* Watch out for both [p] and the start of clean-up *)
      Lwt.choose [p >>= Lwt.return_ok; clean_up_starts >>= Lwt.return_error])
    (function
      | Ok v ->
          ( match Lwt.state clean_up_starts with
          | Sleep ->
              ()
          | _ ->
              assert false ) ;
          unset_handler int_handler ;
          unset_handler term_handler ;
          Lwt.return (Ok v)
      | Error status ->
          ( match Lwt.state clean_up_starts with
          | Return s ->
              assert (s = status)
          | _ ->
              assert false ) ;
          Lwt.cancel p ;
          wait_for_clean_up max_clean_up_time
          >>= fun () ->
          unset_handler int_handler ;
          unset_handler term_handler ;
          Lwt.return (Error status))
    (function
      | Exit -> (
          (* When [Exit] bubbles from the wrapped promise, maybe it called
                [exit_and_raise] *)
          Lwt.pause ()
          >>= fun () ->
          match Lwt.state clean_up_starts with
          | Return status ->
              wait_for_clean_up max_clean_up_time
              >>= fun () ->
              unset_handler int_handler ;
              unset_handler term_handler ;
              Lwt.return (Error status)
          | Fail _ ->
              assert false
          | Sleep ->
              exit 2 ;
              Format.eprintf
                "Exit: exit because of uncaught exception: %s\n%!"
                (Printexc.to_string Exit) ;
              wait_for_clean_up max_clean_up_time
              >>= fun () ->
              unset_handler int_handler ;
              unset_handler term_handler ;
              Lwt.return (Error 2) )
      | exc ->
          exit 2 ;
          Format.eprintf
            "Exit: exit because of uncaught exception: %s\n%!"
            (Printexc.to_string exc) ;
          wait_for_clean_up max_clean_up_time
          >>= fun () ->
          unset_handler int_handler ;
          unset_handler term_handler ;
          Lwt.return (Error 2))

(* same but exit on error *)
let wrap_and_exit ?max_clean_up_time p =
  wrap_and_error ?max_clean_up_time p
  >>= function Ok v -> Lwt.return v | Error status -> Stdlib.exit status

(* same but just return exit status *)
let wrap_and_forward ?max_clean_up_time p =
  wrap_and_error ?max_clean_up_time p
  >>= function Ok v -> Lwt.return v | Error status -> Lwt.return status
