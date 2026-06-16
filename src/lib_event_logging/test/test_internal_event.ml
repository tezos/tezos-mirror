(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Internal_event
    Invocation:   dune exec src/lib_event_logging/test/main.exe
    Subject:      Behavior of Simple.emit when the emission is cancelled
*)

(* A sink whose [handle] fails with [Lwt.Canceled] as an exception (via
   [Lwt.fail]).  This is distinct from a *cancelled* promise: [Lwt.try_bind]'s
   exception handler is only invoked when the inner promise *fails* with an
   exception — [Lwt.Canceled] reaches it exactly this way during node shutdown
   when the internal Lwt_log logger raises it on a closed channel. *)
module Canceled_fail_sink : Internal_event.SINK with type t = unit = struct
  type t = unit

  let uri_scheme = "test-canceled-fail-sink"

  let configure _ = Lwt.return_ok ()

  let should_handle ?section:_ () _ = true

  let handle () _ ?section:_ _ = Lwt.fail Lwt.Canceled

  let close () = Lwt.return_ok ()
end

(* A sink that fails with a plain exception, used to verify that genuine
   failures are still printed to stderr after the patch. *)
module Failing_sink : Internal_event.SINK with type t = unit = struct
  type t = unit

  let uri_scheme = "test-failing-sink"

  let configure _ = Lwt.return_ok ()

  let should_handle ?section:_ () _ = true

  let handle () _ ?section:_ _ = Lwt.fail (Failure "test failure from sink")

  let close () = Lwt.return_ok ()
end

let () =
  Internal_event.All_sinks.register (module Canceled_fail_sink) ;
  Internal_event.All_sinks.register (module Failing_sink)

let test_event =
  Internal_event.Simple.declare_0
    ~section:["test"; "internal_event"]
    ~name:"test_emit_cancellation"
    ~msg:"test event for cancellation testing"
    ()

(* Temporarily redirect [Format.err_formatter] to a buffer while [f] runs. *)
let with_captured_stderr f =
  let buf = Buffer.create 64 in
  let orig_out, orig_flush =
    Format.pp_get_formatter_output_functions Format.err_formatter ()
  in
  Format.pp_set_formatter_output_functions
    Format.err_formatter
    (fun s pos len -> Buffer.add_substring buf s pos len)
    (fun () -> ()) ;
  Lwt.bind
    (Lwt.finalize f (fun () ->
         Format.pp_set_formatter_output_functions
           Format.err_formatter
           orig_out
           orig_flush ;
         Lwt.return_unit))
    (fun () ->
      Format.pp_print_flush Format.err_formatter () ;
      Lwt.return (Buffer.contents buf))

let activate_sink scheme =
  Lwt.bind
    (Internal_event.All_sinks.activate (Uri.of_string (scheme ^ "://test")))
    (function
      | Ok () -> Lwt.return_unit
      | Error e ->
          Test.fail
            "Failed to activate sink %s: %a"
            scheme
            Error_monad.pp_print_trace
            e)

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Simple.emit: Lwt.Canceled does not print to stderr"
    ~tags:["internal_event"; "emit"; "cancel"]
  @@ fun () ->
  Lwt.bind (activate_sink "test-canceled-fail-sink") @@ fun () ->
  Lwt.bind
    (with_captured_stderr (fun () -> Internal_event.Simple.emit test_event ()))
  @@ fun output ->
  Lwt.bind (Internal_event.All_sinks.close ()) @@ fun _ ->
  if output <> "" then
    Test.fail "Expected no stderr output from Lwt.Canceled emit, got: %S" output ;
  Lwt.return_unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Simple.emit: non-Canceled exceptions are still reported on stderr"
    ~tags:["internal_event"; "emit"; "exception"]
  @@ fun () ->
  Lwt.bind (activate_sink "test-failing-sink") @@ fun () ->
  Lwt.bind
    (with_captured_stderr (fun () -> Internal_event.Simple.emit test_event ()))
  @@ fun output ->
  Lwt.bind (Internal_event.All_sinks.close ()) @@ fun _ ->
  if output = "" then
    Test.fail "Expected stderr output for non-Canceled exception, got none" ;
  Lwt.return_unit
