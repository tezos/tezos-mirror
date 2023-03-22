(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Tezos_event_logging_test_helpers

(** [tztest] contains the definition of [tztest]: a wrapper for tests
   cases running in the error monad. It also adds helpers for working
   with the mock sink.  *)

(** Transform a function running in the error monad into an Alcotest, taking
    care of failing the test if the function results is an error.
    Note that the given function must still take care of test assertions,
    e.g. by using [Alcotest.check]. *)
let tztest (name : string) (speed : Alcotest.speed_level) (f : unit -> 'a Lwt.t)
    : unit Alcotest_lwt.test_case =
  Alcotest_lwt.test_case name speed (fun _sw () ->
      let open Lwt_syntax in
      let* r = f () in
      match r with
      | Ok () -> Lwt.return_unit
      | Error err ->
          let* () = Tezos_base_unix.Internal_event_unix.close () in
          let msg = Format.asprintf "@\n%a@." pp_print_trace err in
          Lwt.fail (Alcotest.fail msg))

let tztest_qcheck ?count ~name generator f =
  let name, speed, run =
    QCheck_alcotest.to_alcotest
      ( QCheck.Test.make ?count ~name generator @@ fun x ->
        match Lwt_main.run (f x) with
        | Ok _ -> true
        | Error err -> QCheck.Test.fail_reportf "@\n%a@." pp_print_trace err )
  in
  Alcotest_lwt.test_case name speed (fun _sw () -> Lwt.return @@ run ())

let tztest_qcheck2 ?print ?count ~name generator f =
  let name, speed, run =
    QCheck_alcotest.to_alcotest
      ( QCheck2.Test.make ?print ?count ~name generator @@ fun x ->
        match Lwt_main.run (f x) with
        | Ok _ -> true
        | Error err -> QCheck2.Test.fail_reportf "@\n%a@." pp_print_trace err )
  in
  Alcotest_lwt.test_case name speed (fun _sw () -> Lwt.return @@ run ())

let mock_sink : Mock_sink.t Internal_event.sink_definition =
  (module Mock_sink : Internal_event.SINK with type t = Mock_sink.t)

(** [with_empty_mock_sink f] executes f after activating or clearing a Mock_sink
    sink.

    Sinks can only be registered and activated once, and not removed thereafter.
*)
let with_empty_mock_sink (f : unit -> unit Lwt.t) : unit Lwt.t =
  let open Lwt_syntax in
  if not (Mock_sink.is_activated ()) then (
    Internal_event.All_sinks.register mock_sink ;
    let* r = Internal_event.All_sinks.activate (Uri.of_string "mock-log://") in
    match r with
    | Ok _ -> f ()
    | Error errors ->
        Format.printf
          "Could not initialize mock sink:\n   %a\n"
          pp_print_trace
          errors ;
        Format.print_flush () ;
        Lwt.return_unit)
  else (
    Mock_sink.clear_events () ;
    f ())
