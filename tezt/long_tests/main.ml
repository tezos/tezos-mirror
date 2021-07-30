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
   Component: All
   Invocation: dune exec tezt/long_tests/main.exe
   Subject: This file is the entrypoint of all long Tezt tests. It dispatches to
            other files. Long tests do not run on the CI but on a custom
            architecture which is particularly suited for performance regression tests.
*)

(* This module runs the tests implemented in all other modules of this directory.
   Each module defines tests which are thematically related,
   as functions to be called here. *)

let () =
  Long_test.update_grafana_dashboard
    {
      uid = "longtezts";
      title = "Long Tezts";
      description = "Measurements from tests in tezt/long_tests.";
      panels =
        [
          Row "Test: rpc";
          Grafana.simple_graph "rpc" "duration";
          Row "Test: dummy long test for demo";
          Grafana.simple_graph "random sleep" "duration";
        ];
    }

let dummy_test =
  Long_test.register
    ~__FILE__
    ~title:"dummy long test"
    ~tags:["dummy"]
    ~timeout:(Hours 1)
  @@ fun () ->
  Long_test.time_lwt "random sleep" @@ fun () ->
  Lwt_unix.sleep (Random.float 0.5)

let () =
  Random.self_init () ;
  Test.run ()
