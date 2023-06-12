(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component: Client
   Invocation: dune exec tezt/long_tests/main.exe -- --file logging.ml
   Subject: check regressions in the duration it takes to emit log events.
*)

open Internal_event

let grafana_panels titles : Grafana.panel list =
  Row "Test: client"
  :: List.map
       (fun title ->
         Grafana.simple_graph
           ~measurement:title
           ~test:title
           ~field:"duration"
           ())
       titles

let simple_event name level =
  Simple.declare_2
    ~section:["abc"; "def"]
    ~name
    ~msg:"Hello {id}, {number}"
    ~level
    ("id", Data_encoding.string)
    ~pp1:Format.pp_print_string
    ("number", Data_encoding.int31)
    ~pp2:Format.pp_print_int

let event_i = simple_event "info" Info

let event_n = simple_event "notice" Notice

module Legacy_logging_test = Legacy_logging.Make (struct
  let name = "legacy-test"
end)

let id = "TEST"

let num = 52

(* This should take around 1s per test *)
let nb_repeat = 10_000_000

let test_simple_event_logging_time ~executors title simple_event =
  Long_test.register
    ~__FILE__
    ~title
    ~tags:["simple"; "event"; "logging"; "time"; title]
    ~timeout:(Seconds 10)
    ~executors
  @@ fun () ->
  Long_test.time_lwt title @@ fun () ->
  repeat nb_repeat @@ fun () -> Simple.emit simple_event (id, 52)

let test_legacy_event_logging_time ~executors title f =
  Long_test.register
    ~__FILE__
    ~title
    ~tags:["legacy"; "event"; "logging"; "time"; title]
    ~timeout:(Seconds 10)
    ~executors
  @@ fun () ->
  Long_test.time_lwt title @@ fun () -> repeat nb_repeat f

module Test_sink : SINK = struct
  type t = unit

  let uri_scheme = "test"

  let configure _ = Lwt.return_ok ()

  let should_handle (type a) ?section:_ _ m =
    let module M = (val m : EVENT_DEFINITION with type t = a) in
    M.level = Notice

  let handle _ _ ?section:_ _ = Lwt.return_ok ()

  let close _ = Lwt.return_ok ()
end

let () =
  All_sinks.register (module Test_sink) ;
  Lwt_main.run
  @@ let* x = All_sinks.activate (Uri.of_string "test://") in
     match x with Error _ -> assert false | Ok () -> unit

let sei, sen, lei, len =
  ( "simple_event_info",
    "simple_event_notice",
    "legacy_event_info",
    "legacy_event_notice" )

let grafana_panels = grafana_panels [sei; sen; lei; len]

let register ~executors () =
  test_simple_event_logging_time ~executors sei event_i ;
  test_simple_event_logging_time ~executors sen event_n ;
  test_legacy_event_logging_time ~executors lei (fun () ->
      Legacy_logging_test.lwt_log_info "legacy_event") ;
  test_legacy_event_logging_time ~executors len (fun () ->
      Legacy_logging_test.lwt_log_notice "legacy_event")
