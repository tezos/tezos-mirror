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

(** a simple scenario using a version of the tx_kernel from integration tests*)
let scenario_tx_kernel_deposit_then_withdraw_to_same_address_ name kernel =
  Scenario.make_scenario
    name
    kernel
    [
      Scenario.make_scenario_step
        "incorrect input"
        (Scenario.exec_on_message "incorrect");
      Scenario.make_scenario_step
        "Deposit"
        (Scenario.exec_on_message_from_file Messages.Old.deposit);
      Scenario.make_scenario_step
        "Withdraw"
        (Scenario.exec_on_message_from_file Messages.Old.withdrawal);
    ]

let scenario_tx_kernel_deposit_then_withdraw_to_same_address_no_sig =
  scenario_tx_kernel_deposit_then_withdraw_to_same_address_
    "tx_kernel - deposit_then_withdraw_to_same_address NOSIG"
    Kernels.tx_kernel_vRAM_nosig

let scenario_tx_kernel_deposit_then_withdraw_to_same_address_sig =
  scenario_tx_kernel_deposit_then_withdraw_to_same_address_
    "tx_kernel - deposit_then_withdraw_to_same_address SIG"
    Kernels.tx_kernal_vRam_latest

let scenario_tx_kernel_deposit_transfer_withdraw =
  Scenario.make_scenario
    "tx_kernel - deposit_transfer_withdraw"
    Kernels.tx_kernal_vRam_latest
    [
      Scenario.make_scenario_step
        "First Deposit"
        (Scenario.exec_on_message_from_file
           Messages.Deposit_transfer_withdraw.fst_deposit);
      Scenario.make_scenario_step
        "Second Deposit"
        (Scenario.exec_on_message_from_file
           Messages.Deposit_transfer_withdraw.snd_deposit);
      Scenario.make_scenario_step
        "Invalid Message"
        (Scenario.exec_on_message_from_file
           Messages.Deposit_transfer_withdraw.invalid_message);
      Scenario.make_scenario_step
        "Valid Message"
        (Scenario.exec_on_message_from_file
           Messages.Deposit_transfer_withdraw.valid_message);
    ]

let scenario_tx_kernel_deposit_transfer_withdraw_many_transfers =
  Scenario.make_scenario
    "tx_kernel - deposit_transfer_withdraw_many_transfers"
    Kernels.tx_kernal_vRam_latest
    [
      Scenario.make_scenario_step
        "First Deposit"
        (Scenario.exec_on_message_from_file
           Messages.Deposit_transfer_withdraw.fst_deposit);
      Scenario.make_scenario_step
        "Second Deposit"
        (Scenario.exec_on_message_from_file
           Messages.Deposit_transfer_withdraw.snd_deposit);
      Scenario.make_scenario_step
        "many transfers between two actors"
        (Scenario.exec_on_message_from_file Messages.Large.transfer_two_actors);
    ]

let scenario_computation_kernel =
  Scenario.make_scenario
    "computation kernel"
    Kernels.computation_kernel
    [
      Scenario.make_scenario_step
        "Dummy Message"
        (Scenario.exec_on_message "dummy");
    ]

let scenario_unreachable_kernel =
  Scenario.make_scenario
    "unreachable kernel"
    Kernels.unreachable_kernel
    [
      Scenario.make_scenario_step
        "Dummy Message"
        (Scenario.exec_on_message "dummy");
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
         scenario_unreachable_kernel;
         scenario_computation_kernel;
         scenario_tx_kernel_deposit_then_withdraw_to_same_address_no_sig;
         scenario_tx_kernel_deposit_then_withdraw_to_same_address_sig;
         scenario_tx_kernel_deposit_transfer_withdraw;
         scenario_tx_kernel_deposit_transfer_withdraw_many_transfers;
       ]
