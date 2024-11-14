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

let team = Team.infrastructure

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

let id = "TEST"

(* This should take around 1s per test *)
let nb_repeat = 10_000_000

let test_simple_event_logging_time ~executors title simple_event =
  Long_test.register
    ~__FILE__
    ~title
    ~tags:["simple"; "event"; "logging"; "time"; title]
    ~timeout:(Seconds 10)
    ~uses_node:true
    ~uses_admin_client:true
    ~uses_client:true
    ~team
    ~executors
  @@ fun () ->
  (* Even with [nb_repeat = 10_000_000], the measurements are not very precise:
     we observed some alerts where the duration was up to 33% longer than the average.
     We thus use a threshold of [margin = 0.5] to avoid alert fatigue. *)
  Long_test.time_and_check_regression_lwt title ~margin:0.5 @@ fun () ->
  repeat nb_repeat @@ fun () -> Simple.emit simple_event (id, 52)

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

let sei, sen = ("simple_event_info", "simple_event_notice")

let grafana_panels = grafana_panels [sei; sen]

let register ~executors () =
  test_simple_event_logging_time ~executors sei event_i ;
  test_simple_event_logging_time ~executors sen event_n
