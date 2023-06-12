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

(* Warning: Please, be sure this function is called at first before using
   any function from [Long_test] to avoid undesired behaviour regarding
   the loading of the configuration. *)
let () = Long_test.init ()

let () =
  Long_test.update_grafana_dashboard
    {
      uid = "longtezts";
      title = "Long Tezts";
      description = "Measurements from tests in tezt/long_tests.";
      panels =
        Prt_client.grafana_panels @ Block_validation.grafana_panels
        @ Tenderbake.grafana_panels @ Logging.grafana_panels
        @ Pipelining.grafana_panels @ Tezt_load_time.grafana_panels;
    }

(* Executor for tests that don't take that long to run.
   Very long tests (e.g. tests that take days to run) should run on their own executor. *)
let default_executors = Long_test.[x86_executor1]

let () =
  (* Register your tests here. *)
  (* This test depends on [Tezos_protocol_alpha.*] Tezos libraries *)
  Qcheck_rpc.register_for_alpha ~executors:default_executors () ;
  Prt_client.register ~executors:default_executors ~protocols:[Alpha] ;
  Sc_rollup.register ~executors:default_executors ~protocols:[Alpha] ;
  Script_cache.register ~executors:default_executors ~protocols:[Alpha] ;
  Block_validation.register ~executors:default_executors () ;
  Block_validation.register_semantic_regression_test
    ~executors:[Long_test.block_replay_executor]
    () ;
  Tenderbake.register ~executors:default_executors () ;
  Logging.register ~executors:default_executors () ;
  Pipelining.register ~executors:default_executors () ;
  Tezt_load_time.register ~executors:default_executors () ;
  (* [Test.run] must be the last function to be called. *)
  Test.run ()
