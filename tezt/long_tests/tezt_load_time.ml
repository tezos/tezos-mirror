(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component: Tezt
   Invocation: dune exec tezt/long_tests/main.exe -- --file tezt_load_time.ml
   Subject: check regressions in the duration it takes for the main tezt test suite to load.
*)

let tezt_load_time = "tezt load time"

let grafana_panels : Grafana.panel list =
  [
    Row "Test: Tezt";
    Grafana.simple_graph
      ~measurement:tezt_load_time
      ~test:tezt_load_time
      ~field:"duration"
      ();
  ]

let test_tezt_tests_suite_load_time ~executors () =
  Long_test.register
    ~__FILE__
    ~title:tezt_load_time
    ~tags:["client"; "load"]
    ~timeout:(Minutes 2)
    ~executors
  @@ fun () ->
  let* () = Process.run "dune" ["build"; "tezt/tests/main.exe"] in
  Long_test.time_lwt ~repeat:5 tezt_load_time @@ fun () ->
  let* () =
    (* [--list] will ensure that we register all tests, and so get a
       reasonable estimation of start up time. [--only 1] is passed to
       make the output terser when running the test with debug
       output. *)
    Process.run "_build/default/tezt/tests/main.exe" ["--list"; "--only"; "1"]
  in
  unit

let register ~executors () = test_tezt_tests_suite_load_time ~executors ()
