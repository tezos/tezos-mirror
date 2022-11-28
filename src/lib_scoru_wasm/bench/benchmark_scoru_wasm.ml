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

open Inputs

let scenario_tx_kernel_deposit_transfer_withdraw_LOOP =
  Scenario.make_scenario
    "tx_kernel - deposit_transfer_withdraw"
    Kernels.tx_kernal_vRam_latest
    [
      Scenario.make_scenario_step
        "Load inbox"
        (Scenario.load_messages
           1l
           [
             Deposit (File Messages.Deposit_transfer_withdraw.fst_deposit);
             Deposit (File Messages.Deposit_transfer_withdraw.snd_deposit);
             Other (File Messages.Deposit_transfer_withdraw.invalid_message);
             Other (File Messages.Deposit_transfer_withdraw.valid_message);
           ]);
      Scenario.make_scenario_step "Loop" Scenario.exec_loop;
    ]

let scenario_tx_kernel_deposit_transfer_withdraw_many_transfers_LOOP =
  Scenario.make_scenario
    "tx_kernel - deposit_transfer_withdraw_many_transfers"
    Kernels.tx_kernal_vRam_latest
    [
      Scenario.make_scenario_step
        "Load inbox"
        (Scenario.load_messages
           1l
           [
             Deposit (File Messages.Deposit_transfer_withdraw.fst_deposit);
             Deposit (File Messages.Deposit_transfer_withdraw.snd_deposit);
             Other (File Messages.Large.transfer_two_actors);
           ]);
      Scenario.make_scenario_step "Loop" Scenario.exec_loop;
    ]

let scenario_computation_kernel =
  Scenario.make_scenario
    "computation kernel"
    Kernels.computation_kernel
    [
      Scenario.make_scenario_step
        "Load inbox"
        (Scenario.load_messages 1l [Other (Str "dummy")]);
      Scenario.make_scenario_step "Loop" Scenario.exec_loop;
    ]

let scenario_unreachable_kernel =
  Scenario.make_scenario
    "unreachable kernel"
    Kernels.unreachable_kernel
    [
      Scenario.make_scenario_step
        "Load inbox"
        (Scenario.load_messages 1l [Other (Str "dummy")]);
      Scenario.make_scenario_step "Loop" Scenario.exec_loop;
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
       ~totals:false
       ~irmin:false
       (filename ())
       [
         Scenario.ignore_scenario scenario_unreachable_kernel;
         Scenario.ignore_scenario scenario_computation_kernel;
         scenario_tx_kernel_deposit_transfer_withdraw_LOOP;
         scenario_tx_kernel_deposit_transfer_withdraw_many_transfers_LOOP;
       ]
