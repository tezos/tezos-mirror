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

(* The aim of this test is to check that Lwt_Exit can handle all the signals
   that POSIX say are handleable. *)

open Lwt.Infix

let devnull = Lwt_main.run (Lwt_unix.openfile "/dev/null" [O_WRONLY] 0)

let single_signal_setup signal =
  Lwt_exit.make_signal_setup ~soft:[] ~hard:[signal]

let rec child_loop () = Lwt_unix.sleep 0.1 >>= child_loop

let child signal =
  (* redirecting stderr to devnull to avoid polluting stderr with messages when
     the child process is killed. These messages make the test look like it is
     failing because they, obviously, look error-like. *)
  Lwt_unix.dup2 devnull Lwt_unix.stderr ;
  let signal_setup = single_signal_setup signal in
  Stdlib.exit @@ Lwt_main.run
  @@ Lwt_exit.wrap_and_exit ~signal_setup
  @@ child_loop ()

let test_one_signal (signal, name) =
  (* tests that a process that sets up a hard-handler for [signal] exits as
     expected when receiving a signal. *)
  match Lwt_unix.fork () with
  | 0 ->
      child signal
  | pid -> (
      Lwt_main.run
      @@
      let (_ : unit Lwt.t) =
        Lwt_unix.sleep 0.01 >>= fun () -> Unix.kill pid signal ; Lwt.return ()
      in
      Lwt_unix.waitpid [] pid
      >|= fun (_, status) ->
      match status with
      | WEXITED s when s = 128 lor 64 ->
          ()
      | WEXITED s ->
          Format.kasprintf failwith "WEXITED(%d) on %s" s name
      | WSIGNALED s ->
          Format.kasprintf failwith "WSIGNALED(%d) on %s" s name
      | WSTOPPED s ->
          Format.kasprintf failwith "WSTOPPED(%d) on %s" s name )

let main () =
  let open Sys in
  List.iter
    test_one_signal
    [ (sigalrm, "ALRM");
      (sigchld, "CHLD");
      (sigabrt, "ABRT");
      (sigfpe, "FPE");
      (sighup, "HUP");
      (sigill, "ILL");
      (sigint, "INT");
      (sigpipe, "PIPE");
      (sigquit, "QUIT");
      (sigsegv, "SEGV");
      (sigterm, "TERM");
      (sigusr1, "USR1");
      (sigusr2, "USR2");
      (sigcont, "CONT");
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

(** Non-catchable signals
    (sigkill, "KILL");
    (sigstop, "STOP");
    *)

let () = main ()
