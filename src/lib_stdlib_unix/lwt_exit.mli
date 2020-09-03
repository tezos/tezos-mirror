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

(** [Lwt_exit] provides helpers to handle:

    - OS signals,
    - cleaning-up before exiting, and
    - exiting.

    Specifically, this module allows users to (1) register clean-up callbacks
    and (2) trigger a soft exit. When a soft exit is triggered, the clean-up
    callbacks are called. The process exits once all the clean-up callbacks
    calls have resolved. *)

(** A global promise that resolves when clean-up starts. Note that there is no
    way to "just" start clean-up. Specifically, it is only possible to start the
    clean-up as a side-effect of triggering an exit. *)
val clean_up_starts : int Lwt.t

(** A global promise that resolves when clean-up ends. *)
val clean_up_ends : int Lwt.t

(** Attaching and detaching callbacks. *)

type clean_up_callback_id

(** [register_clean_up_callback f] registers [f] to be called as part of the
    clean-up. Typically this is used to flush outputs, rollback/commit pending
    changes, gracefully close connections with peers, etc.

    The call to [f] receives an argument [n] that indicates the status the
    process will exit with at the end of clean-up: [0] is for success, [1] for
    interruption by signals, [2] for uncaught exceptions, other values are also
    available.

    The argument [after], if passed, delays the call to this callback until
    the one identified by [after] has resovled.

    Once clean-up has started, this function has no effect.

    The promise returned by this callback may be canceled if it takes too long
    to complete. (See [max_clean_up_time] below.) *)
val register_clean_up_callback :
  ?after:clean_up_callback_id ->
  loc:string ->
  (int -> unit Lwt.t) ->
  clean_up_callback_id

(** [unregister_clean_up_callback cid] removes the callback with id [cid] from
    the set of functions to call for cleaning up.

    Once clean-up has started, this function has no effect. *)
val unregister_clean_up_callback : clean_up_callback_id -> unit

(** Example use:

   [let p = open_resource r in
    let clean_p = register_clean_up_callback (fun _ -> close_resource p) in
    let rec feed () =
       read () >>= fun v ->
       push_to_resource p >>= fun () ->
       feed ()
    in
    feed () >>= fun () ->
    close_resource p >>= fun () ->
    unregister_clean_up_callback clean_p;
    Lwt.return ()]
*)

(** [exit_and_raise n] triggers a soft exit (including clean-up) and raises
    [Exit]. This is intended for use deep inside the program, at a place that
    wants to trigger an exit after observing, say, a fatal error. *)
val exit_and_raise : int -> 'a

(** [exit_and_wait n] triggers a soft exit (including clean-up) and stays
    pending until it is finished. This is intended to be used directly within
    [Lwt_main.run] for a clean exit. *)
val exit_and_wait : int -> int Lwt.t

(** Managing signals *)

(** A soft signal handler is one that triggers clean-up.

    After the clean-up has started, and after a safety period has elapsed,
    sending the same soft-handled signal a second time terminates the
    process immediately. The safety period is set by the parameter
    [double_signal_safety] of the [wrap_and_*] functions (below).

    A hard signal handler is one that terminates the process immediately.

    IMPORTANT: a hard exit can leave open files in inconsistent states. *)

type signal_setup

(** [make_signal_setup ~soft ~hard] is a signal setup with [soft] as soft
    signals and [hard] as hard signals.

    @raise [Invalid_argument] if a signal is not one of [Sys.sig*]*)
val make_signal_setup : soft:int list -> hard:int list -> signal_setup

(** [default_signal_setup] is
    [make_signal_setup ~soft:[Sys.sigint; Sys.sigterm] ~hard:[]].

    Note that pressing Ctrl-C sends [SIGINT] to the process whilst shutting it
    down through systemd sends [SIGTERM]. This is the reasoning behind the
    default: both of those signals should be handled softly in most cases. *)
val default_signal_setup : signal_setup

(** [signal_name signal] is the name of [signal].
    E.g., [signal_name Sys.sigterm] is ["TERM"]. *)
val signal_name : int -> string

(** [wrap_and_exit p] is a promise [q] that behaves as follows:

    If [exit_and_raise] is called before [p] is resolved, then the process
    terminates as soon as the clean-up has ended. As a result, the [q] never
    resolves.

    If [p] is fulfilled with value [v] (and [exit_and_raise] was not called)
    then [q] also is fulfilled with [v].

    If [p] is rejected (and [exit_and_raise] was not called), a soft-exit with
    status [2] is triggered and the process terminates as soon as the clean-up
    has ended.

    In addition, [wrap_and_exit p] sets up the signal handlers described above.
    This can cause calls to [exit_and_raise].

    The optional argument [max_clean_up_time] limits the time the clean-up phase
    is allowed to run for. If any of the clean-up callbacks is still pending
    when [max_clean_up_time] has elapsed, then the pending callbacks are
    [cancel]ed, then, after a [Lwt.pause], the process exits.

    The optional argument [double_signal_safety] (defaults to one (1) second)
    is the grace period after sending one of the softly-handled signal before
    sending the same signal is handled as hard.

    The optional argument [signal_setup] (defaults to [default_signal_setup])
    sets up soft and hard handlers at the beginning and clears them when [q]
    resolves.

    Intended use:
    [Stdlib.exit @@ Lwt_main.run begin
      Lwt_exit.wrap_and_exit (init ()) >>= fun v ->
      Lwt_exit.wrap_and_exit (main v) >>= fun v ->
      Lwt_exit.wrap_and_exit (shutdown v) >>= fun () ->
      exit_and_wait 0 (* clean exit afterwards *)
    end]
*)
val wrap_and_exit :
  ?signal_setup:signal_setup ->
  ?double_signal_safety:Ptime.Span.t ->
  ?max_clean_up_time:Ptime.Span.t ->
  'a Lwt.t ->
  'a Lwt.t

(** [wrap_and_error p] is similar to [wrap_and_exit p] but it resolves to
    [Error status] instead of exiting with [status]. When it resolves with
    [Error _] (i.e., if a soft-exit has been triggered), clean-up has already
    ended.

    Intended use:
    [Stdlib.exit @@ Lwt_main.run begin
      Lwt_exit.wrap_and_error (init ()) >>= function
      | Error exit_status ->
        Format.eprintf "Initialisation failed\n%!";
        Lwt.return exit_status
      | Ok v ->
        Lwt_exit.wrap_and_error (main v) >>= function
        | Error exit_status ->
          Format.eprintf "Processing failed\n%!";
          Lwt.return exit_status
        | Ok v ->
          Lwt_exit.wrap_and_error (shutdown ()) >>= function
          | Error exit_status ->
            Format.eprintf "Shutdown failed\n%!";
            Lwt.return exit_status
          | Ok () ->
            exit_and_wait 0 >>= fun _ ->
            Lwt.return 0
    end]
*)
val wrap_and_error :
  ?signal_setup:signal_setup ->
  ?double_signal_safety:Ptime.Span.t ->
  ?max_clean_up_time:Ptime.Span.t ->
  'a Lwt.t ->
  ('a, int) result Lwt.t

(** [wrap_and_forward p] is similar to [wrap_and_error p] except that it
    collapses [Ok _] and [Error _].

    Note that, in general, you can expect the status [0] to come from a
    successfully resolved [p]. However, It could also be because of a soft-exit
    with status [0]. As a result, you cannot be certain, based on the status
    alone, whether clean-up callbacks have been called.

    Intended use:
    [Stdlib.exit @@ Lwt_main.run begin
      Lwt_exit.wrap_and_forward (main ()) >>= function
      | 0 ->
        Format.printf "I'm done, bye!\n%!";
        Lwt.return 0
      | 1 -> (* signaling *)
        Format.printf "Shutdown complete\n";
        Lwt.return 1
      | 2 -> (* uncaught exception *)
        Format.printf "An error occurred.\n";
        Format.printf "Please check %s\n" log_file;
        Format.printf "And consider reporting the issue\n%!";
        Lwt.return 2
      | _ -> assert false
    end]
*)
val wrap_and_forward :
  ?signal_setup:signal_setup ->
  ?double_signal_safety:Ptime.Span.t ->
  ?max_clean_up_time:Ptime.Span.t ->
  int Lwt.t ->
  int Lwt.t
