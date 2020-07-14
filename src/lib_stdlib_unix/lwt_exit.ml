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

type callback = {
  callback : int -> unit Lwt.t;
  after : clean_up_callback_id list;
  loc : string;
}

let clean_up_callbacks : callback Callbacks_map.t ref = ref Callbacks_map.empty

(* adding and removing clean-up callbacks affects the global reference map *)
let register_clean_up_callback ?(after = []) ~loc callback =
  let id = new_clean_up_callback_id () in
  let callback = {callback; after; loc} in
  clean_up_callbacks := Callbacks_map.add id callback !clean_up_callbacks ;
  id

let unregister_clean_up_callback id =
  clean_up_callbacks := Callbacks_map.remove id !clean_up_callbacks

(* 2. clean-up *)

(* cleaning-up is just calling all the clean-up callbacks, note that the
   function is not exported: it cannot be called directly, it can only be
   triggered as a side effect to calling [exit_and_raise] or [exit_and_wait]

   Returns a seq of clean-up promises along with their identifiers. *)
let clean_up status =
  (* NOTE: [to_seq] iterates in increasing order of keys *)
  let callbacks = Callbacks_map.to_seq !clean_up_callbacks in
  clean_up_callbacks := Callbacks_map.empty ;
  let promises : (string * unit Lwt.t) Callbacks_map.t =
    Seq.fold_left
      (fun promises (id, {callback; after; loc}) ->
        let pre =
          match after with
          | [] ->
              Lwt.return_unit
          | _ :: _ as after -> (
            match
              Callbacks_map.fold
                (fun id (_, p) ps -> if List.mem id after then p :: ps else ps)
                promises
                []
            with
            | [] ->
                (* This can happen if all after-callbacks were unregistered *)
                Lwt.return_unit
            | [p] ->
                p
            | _ :: _ :: _ as ps ->
                Lwt.join ps )
        in
        let promise = pre >>= fun () -> callback status in
        Lwt.on_failure promise (fun exc ->
            Format.eprintf
              "(%s) Exit: uncaught exception during clean-up (%s): %s\n%!"
              Sys.executable_name
              loc
              (Printexc.to_string exc)) ;
        Callbacks_map.add id (loc, promise) promises)
      Callbacks_map.empty
      callbacks
  in
  Seq.map snd @@ Callbacks_map.to_seq promises

(* 3. synchronisation primitives *)

(* [clean_up_starts] an exported promise that resolves when the clean-up starts.
   [start_clean_up] a non-exported resolver for the promise.

   Note that the promise is not cancelable and we never pass an exception to the
   resolver. Consequently, the promise cannot be rejected. *)
let (clean_up_starts_internal, start_clean_up) = Lwt.wait ()

(* [clean_up_starts] is exported with a delay to ensure that [wrap_and_*]
   function witness the start of the cleaning up before users of the library. *)
let clean_up_starts =
  Lwt.no_cancel
    ( clean_up_starts_internal
    >>= fun v -> Lwt.pause () >>= fun () -> Lwt.return v )

let clean_up_promises =
  clean_up_starts_internal >>= fun status -> Lwt.return @@ clean_up status

(* [clean_up_ends] is a promise that resolves once the clean-up is finished. *)
let clean_up_ends =
  clean_up_starts_internal
  >>= fun status ->
  clean_up_promises
  >>= fun promises ->
  Lwt.join @@ List.of_seq
  @@ Seq.map
       (fun (_, promise) ->
         Lwt.try_bind
           (fun () -> promise)
           (fun () -> Lwt.return_unit)
           (fun _ -> Lwt.return_unit))
       promises
  >>= fun () -> Lwt.return status

(* 4. exiting *)

(* simple exit is not exported, it is just to factor out exiting *)
let exit n =
  match Lwt.state clean_up_starts_internal with
  | Sleep ->
      Lwt.wakeup start_clean_up n
  | Return _ ->
      ()
  | Fail _ ->
      (* Remember [clean_up_starts_internal] cannot be rejected. *)
      assert false

(* [exit_and_raise] is meant to be used deep inside the program after having
   witnessed, say, a fatal error. It raises an exception so that it can be used
   anywhere in the program. *)
let exit_and_raise n = exit n ; raise Exit

(* [exit_and_wait] is meant to be used near the main invocation of the program,
   right inside of [Lwt_main.run] but presumably after [wrap_and_error]. *)
let exit_and_wait n = exit n ; clean_up_ends

(* exit codes *)

let incomplete_clean_up_mask = 128

let signal_exit_code = 127

let uncaught_exception_exit_code = 126

let mask_code_bc_incomplete_clean_up code = code lor incomplete_clean_up_mask

let mask_code_if_incomplete_clean_up ~complete:all_fine code =
  if all_fine then code else mask_code_bc_incomplete_clean_up code

(* 5. signals *)

type signal_setup = {soft : (int * string) list; hard : (int * string) list}

(** Known signals and their names *)
let all_signal_names =
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
  match List.assoc_opt signal all_signal_names with
  | Some name ->
      name
  | None ->
      Format.asprintf "%d" signal

let make_signal_setup ~soft ~hard =
  try
    let soft = List.map (fun signal -> (signal, signal_name signal)) soft in
    let hard = List.map (fun signal -> (signal, signal_name signal)) hard in
    {soft; hard}
  with Not_found -> raise (Invalid_argument "Lwt_exit.make_signal_setup")

let default_signal_setup =
  make_signal_setup ~soft:[Sys.sigint; Sys.sigterm] ~hard:[]

let sleep_span s = Lwt_unix.sleep (Ptime.Span.to_float_s s)

let set_already_received_once double_signal_safety already_received_once name =
  if Ptime.Span.(equal double_signal_safety zero) then (
    Format.eprintf
      "(%s) %s: send signal again to force-quit.\n%!"
      Sys.executable_name
      name ;
    already_received_once := true )
  else
    Lwt_utils.dont_wait
      (fun _exc -> assert false)
      (fun () ->
        (* Wait one second for safety, then set force-quitting *)
        sleep_span double_signal_safety
        >>= fun () ->
        Format.eprintf
          "(%s) %s: send signal again to force-quit.\n%!"
          Sys.executable_name
          name ;
        already_received_once := true ;
        Lwt.return_unit)

let default_double_signal_safety = Option.get @@ Ptime.Span.of_float_s 1.0

(* soft handling: trigger an exit on first signal, immediately terminate
   process on second signal *)
let set_soft_handler ?(double_signal_safety = default_double_signal_safety)
    signal name =
  let already_received_once = ref false in
  Lwt_unix.on_signal signal (fun _signal ->
      if !already_received_once then (
        Format.eprintf
          "(%s) %s: signal received again, forcing immediate termination.\n%!"
          Sys.executable_name
          name ;
        Stdlib.exit (mask_code_bc_incomplete_clean_up signal_exit_code) )
      else
        match Lwt.state clean_up_starts_internal with
        | Sleep ->
            Format.eprintf
              "(%s) %s: triggering shutdown.\n%!"
              Sys.executable_name
              name ;
            exit signal_exit_code ;
            set_already_received_once
              double_signal_safety
              already_received_once
              name
        | Return _ ->
            Format.eprintf
              "(%s) %s: already in shutdown.\n%!"
              Sys.executable_name
              name ;
            set_already_received_once
              double_signal_safety
              already_received_once
              name
        | Fail _ ->
            (* Remember [clean_up_starts_internal] cannot be rejected. *)
            assert false)

(* hard handling: immediately terminate process *)
let set_hard_handler signal name =
  Lwt_unix.on_signal signal (fun _signal ->
      Format.eprintf "(%s) %s: force-quiting.\n%!" Sys.executable_name name ;
      Stdlib.exit (mask_code_bc_incomplete_clean_up signal_exit_code))

let setup_signal_handlers ?double_signal_safety signal_setup =
  let soft_handler_ids =
    List.fold_left
      (fun acc (signal, name) ->
        set_soft_handler ?double_signal_safety signal name :: acc)
      []
      signal_setup.soft
  in
  let all_handler_ids =
    List.fold_left
      (fun acc (signal, name) -> set_hard_handler signal name :: acc)
      soft_handler_ids
      signal_setup.hard
  in
  all_handler_ids

let unset_handlers = List.iter Lwt_unix.disable_signal_handler

(* 6. internal synchronisation *)

let wait_for_clean_up max_clean_up_time =
  ( match Lwt.state clean_up_starts_internal with
  | Return _ ->
      ()
  | Fail _ ->
      (* Remember [clean_up_starts_internal] cannot be rejected. *)
      assert false
  | Sleep ->
      (* We only call this function after the clean-up has started, and we do
         not export the function *)
      assert false ) ;
  Lwt.pause ()
  >>= fun () ->
  ( match Lwt.state clean_up_promises with
  | Return _ ->
      ()
  | Fail _ ->
      (* the promises are a promise that cannot be rejected *)
      assert false
  | Sleep ->
      (* One tick after the clean-up has started, all the promises have been
         collected. *)
      assert false ) ;
  ( match Lwt.state clean_up_ends with
  | Fail _ ->
      assert false
  | Return _ ->
      (* This happens when there are no callbacks registered: the clean-up is
         immediate. *)
      Lwt.return_unit
  | Sleep -> (
    match max_clean_up_time with
    | None ->
        (* without timeout: just wait *)
        clean_up_ends >>= fun _ -> Lwt.return_unit
    | Some s ->
        (* with timeout: pick first to finish *)
        Lwt.choose [(clean_up_ends >>= fun _ -> Lwt.return_unit); sleep_span s]
    ) )
  (* pause in case timeout and clean-up needs to deal with cancellation *)
  >>= Lwt.pause
  >|= fun () ->
  match Lwt.state clean_up_promises with
  | Lwt.Sleep ->
      (* we have already asserted this earlier in the function *)
      assert false
  | Lwt.Fail _ ->
      (* we have already asserted this earlier in the function *)
      assert false
  | Lwt.Return promises ->
      (* check (and log) whether all clean-up is done successfully *)
      Seq.fold_left
        (fun all_fine (id, promise) ->
          match Lwt.state promise with
          | Lwt.Sleep ->
              (* if a promise has not been given enough time to resolve, then it
                 means it was interupted by a timeout: [max_clean_up_time] *)
              assert (max_clean_up_time <> None) ;
              Format.eprintf
                "(%s) Exit: timeout, clean-up callback not terminated (%s)\n%!"
                Sys.executable_name
                id ;
              false
          | Lwt.Fail exc ->
              Format.eprintf
                "(%s) Exit: clean-up callback failed (%s): %s\n%!"
                Sys.executable_name
                id
                (Printexc.to_string exc) ;
              false
          | Lwt.Return () ->
              all_fine)
        true
        promises

(* 7. main interface: wrapping promises *)

(* take a promise and wrap it in `Ok` but also watch for exiting and wrap that
   in `Error` *)
let wrap_and_error ?(signal_setup = default_signal_setup) ?double_signal_safety
    ?max_clean_up_time p =
  ( match Lwt.state clean_up_starts_internal with
  | Lwt.Fail _ ->
      (* Remember [clean_up_starts_internal] cannot be rejected. *)
      assert false
  | Lwt.Return _ ->
      raise (Invalid_argument "Lwt_exit.wrap")
  | Lwt.Sleep ->
      () ) ;
  match Lwt.state p with
  | Lwt.Fail _ | Lwt.Return _ ->
      p >>= Lwt.return_ok
  | Lwt.Sleep ->
      let handler_ids =
        setup_signal_handlers ?double_signal_safety signal_setup
      in
      Lwt.try_bind
        (fun () ->
          (* Watch out for both [p] and the start of clean-up *)
          Lwt.choose
            [p >>= Lwt.return_ok; clean_up_starts_internal >>= Lwt.return_error])
        (function
          | Ok v ->
              (* In this branch, the [Ok] indicates that [p] was resolved before
                 [clean_up_starts_internal]. As a result,
                 [clean_up_starts_internal] must still be pending.

                 It is only possible for two promises to resolve simultaneously
                 if they are physically equal, if one is a proxy for the other,
                 or some other similar situation. Because
                 [clean_up_starts_internal] is not exported, this is not
                 possible. *)
              assert (Lwt.state clean_up_starts_internal = Lwt.Sleep) ;
              unset_handlers handler_ids ;
              Lwt.return (Ok v)
          | Error status ->
              (* Conversely to the previous comment: when
                 [clean_up_starts_internal] resolves first, then [p] cannot have
                 resolved yet. *)
              assert (Lwt.state clean_up_starts_internal = Lwt.Return status) ;
              Lwt.cancel p ;
              wait_for_clean_up max_clean_up_time
              >>= fun complete ->
              unset_handlers handler_ids ;
              let status = mask_code_if_incomplete_clean_up ~complete status in
              Lwt.return (Error status))
        (function
          | Exit -> (
              (* When [Exit] bubbles from the wrapped promise, maybe it called
                [exit_and_raise] *)
              Lwt.pause ()
              >>= fun () ->
              match Lwt.state clean_up_starts_internal with
              | Return status ->
                  wait_for_clean_up max_clean_up_time
                  >>= fun complete ->
                  unset_handlers handler_ids ;
                  let status =
                    mask_code_if_incomplete_clean_up ~complete status
                  in
                  Lwt.return (Error status)
              | Fail _ ->
                  (* Remember [clean_up_starts_internal] cannot be rejected. *)
                  assert false
              | Sleep ->
                  exit uncaught_exception_exit_code ;
                  Format.eprintf
                    "(%s) Exit: exit because of uncaught exception: %s\n%!"
                    Sys.executable_name
                    (Printexc.to_string Exit) ;
                  wait_for_clean_up max_clean_up_time
                  >>= fun complete ->
                  unset_handlers handler_ids ;
                  let status =
                    mask_code_if_incomplete_clean_up
                      ~complete
                      uncaught_exception_exit_code
                  in
                  Lwt.return (Error status) )
          | exc ->
              exit uncaught_exception_exit_code ;
              Format.eprintf
                "(%s) Exit: exit because of uncaught exception: %s\n%!"
                Sys.executable_name
                (Printexc.to_string exc) ;
              wait_for_clean_up max_clean_up_time
              >>= fun complete ->
              unset_handlers handler_ids ;
              let status =
                mask_code_if_incomplete_clean_up
                  ~complete
                  uncaught_exception_exit_code
              in
              Lwt.return (Error status))

(* same but exit on error *)
let wrap_and_exit ?signal_setup ?double_signal_safety ?max_clean_up_time p =
  wrap_and_error ?max_clean_up_time ?double_signal_safety ?signal_setup p
  >>= function Ok v -> Lwt.return v | Error status -> Stdlib.exit status

(* same but just return exit status *)
let wrap_and_forward ?signal_setup ?double_signal_safety ?max_clean_up_time p =
  wrap_and_error ?max_clean_up_time ?double_signal_safety ?signal_setup p
  >>= function Ok v -> Lwt.return v | Error status -> Lwt.return status
