(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

(** Benchmarking
    -------
    Component:    Wasm PVM
    Invocation:   dune exec src/lib_scoru_wasm/bench/benchmark_scoru_wasm.exe
    Subject:      Measure nb of ticks

    Use the dune option  "--profile=release" when launching a subset of benchs

    Kernels:
    -  src/lib_scoru_wasm/test/wasm_kernels/
    - src/proto_alpha/lib_protocol/test/integration/wasm_kernel/


*)
open Octez_smart_rollup_wasm_benchmark_lib

open Inputs

let reveal_builtins = Pvm_instance.builtins

let scenario_computation_kernel =
  Scenario.make_scenario
    "computation kernel"
    Kernels.computation_kernel
    [
      Scenario.make_scenario_step
        "Load inbox"
        (Scenario.load_messages 1l [Other (Str "dummy")]);
      Scenario.make_scenario_step "Loop" (Scenario.exec_slow ~reveal_builtins);
    ]

let scenario_unreachable_kernel =
  Scenario.make_scenario
    "unreachable kernel"
    Kernels.unreachable_kernel
    [
      Scenario.make_scenario_step
        "Load inbox"
        (Scenario.load_messages 1l [Other (Str "dummy")]);
      Scenario.make_scenario_step "Loop" (Scenario.exec_slow ~reveal_builtins);
    ]

let demo_input_step =
  Scenario.make_scenario_step
    "Load inbox"
    (Scenario.load_messages
       0l
       [
         Encoded (File Messages.Demo.deposit_blue);
         Encoded (File Messages.Demo.deposit_green);
         Encoded (File Messages.Demo.deposit_red)
         (* Other "gen_messages/transfer_0_000"; *);
       ])

let scenario_tx_kernel_demo =
  Scenario.make_scenario
    "tx_kernel - demo scenario"
    Kernels.tx_kernel_dac
    [
      demo_input_step;
      Scenario.make_scenario_step "Loop" (Scenario.exec_slow ~reveal_builtins);
    ]

let scenario_tx_kernel_demo_fast =
  Scenario.make_scenario
    "tx_kernel - demo scenario fast"
    Kernels.tx_kernel_dac
    [
      demo_input_step;
      Scenario.make_scenario_step "Loop" (Scenario.exec_fast ~reveal_builtins);
    ]

let filename () =
  let t = Unix.localtime (Unix.time ()) in
  Printf.sprintf
    "benchmark_WASM_%04d-%02d-%02d_%02dh%02d.csv"
    (1900 + t.tm_year)
    (t.tm_mon + 1)
    t.tm_mday
    t.tm_hour
    t.tm_min

let () =
  Lwt_main.run
  @@ Scenario.run_scenarios
       ~verbose:true
       ~totals:true
       ~irmin:false
       (filename ())
       [
         Scenario.ignore_scenario scenario_unreachable_kernel;
         Scenario.ignore_scenario scenario_computation_kernel;
         scenario_tx_kernel_demo;
         scenario_tx_kernel_demo_fast;
       ]
