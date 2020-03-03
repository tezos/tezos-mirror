(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

exception Exit

let (termination_thread, exit_wakener) = Lwt.wait ()

let exit x = Lwt.wakeup exit_wakener x ; raise Exit

let () =
  Lwt.async_exception_hook :=
    function
    | Exit ->
        ()
    | e ->
        let backtrace = Printexc.get_backtrace () in
        let pp_exn_trace ppf backtrace =
          if String.length backtrace <> 0 then
            Format.fprintf
              ppf
              "@,Backtrace:@,  @[<h>%a@]"
              Format.pp_print_text
              backtrace
        in
        (* TODO Improve this *)
        Format.eprintf
          "@[<v 2>@[Uncaught (asynchronous) exception (%d):@ %s@]%a@]@.%!"
          (Unix.getpid ())
          (Printexc.to_string e)
          pp_exn_trace
          backtrace ;
        Lwt.wakeup exit_wakener 1

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

let set_exit_handler ?(log = Format.eprintf "%s\n%!") signal =
  match List.assoc_opt signal signals with
  | None ->
      Format.kasprintf
        invalid_arg
        "Killable.set_exit_handler: unknown signal %d"
        signal
  | Some name ->
      let handler signal =
        try
          Format.kasprintf
            log
            "Received the %s signal, triggering shutdown."
            name ;
          exit signal
        with _ -> ()
      in
      ignore (Lwt_unix.on_signal signal handler : Lwt_unix.signal_handler_id)

(* Which signals is the program meant to exit on *)
let signals_to_exit_on = ref []

let exit_on ?log signal =
  if List.mem signal !signals_to_exit_on then
    Format.kasprintf
      Stdlib.failwith
      "Killable.exit_on: already registered signal %d"
      signal
  else (
    signals_to_exit_on := signal :: !signals_to_exit_on ;
    set_exit_handler ?log signal )

type outcome = Resolved of int | Exited of int

let retcode_of_unit_result_lwt p =
  let open Lwt.Infix in
  p
  >>= function
  | Error e ->
      (* TODO: print *) ignore e ; Lwt.return 1
  | Ok () ->
      Lwt.return 0

let wrap_promise (p : int Lwt.t) =
  let open Lwt.Infix in
  Lwt.choose
    [(p >|= fun v -> Resolved v); (termination_thread >|= fun s -> Exited s)]
  >>= function
  | Resolved r ->
      Lwt.return r
  | Exited s ->
      (*TODO: what are the correct expected behaviour here?*)
      if List.mem s !signals_to_exit_on then (
        (* Exit because of signal *)
        Lwt.cancel p ; Lwt.return 2 )
      else (* Other exit *)
        Stdlib.exit 3
