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

open Tezos_scoru_wasm
open Pvm_instance
open Wasm_pvm_state
open Wasm

type 'a run_state = {state : 'a; message_counter : int}

let lift_action action =
  let open Lwt_syntax in
  fun run_state ->
    let* state = action run_state.state in
    return {run_state with state}

let lift_lookup lookup =
  let open Lwt_syntax in
  fun run_state ->
    let* res = lookup run_state.state in
    return res

type 'a action = 'a run_state -> 'a run_state Lwt.t

type scenario_step = string * tree action

type scenario = {kernel : string; actions : scenario_step list}

let make_scenario kernel actions = {kernel; actions}

let make_scenario_step (label : string) (action : tree action) : scenario_step =
  (label, action)

let run_action name run_state action =
  let open Lwt_syntax in
  let* before_tick = lift_lookup get_tick_from_tree run_state in
  let* tick_state =
    lift_lookup Wasm.Internal_for_tests.get_tick_state run_state
  in
  let _ =
    Printf.printf
      "=========\n%s \nStart at tick %s %s\n%!"
      name
      (Z.to_string before_tick)
      (PP.tick_label tick_state)
  in
  let* time, run_state = Measure.time (fun () -> action run_state) in
  let* info = lift_lookup Wasm.get_info run_state in
  let* tick_state =
    lift_lookup Wasm.Internal_for_tests.get_tick_state run_state
  in
  let _ = Printf.printf "took %f s\n%!" (Measure.to_seconds time) in
  let _ =
    Printf.printf
      "last tick: %s %s\n%!"
      (Z.to_string info.current_tick)
      (PP.tick_label tick_state)
  in
  return run_state

let exec_on_message message run_state =
  let open Lwt_syntax in
  let* run_state =
    lift_action
      (Exec.set_input_step message run_state.message_counter)
      run_state
  in
  lift_action Exec.eval_until_input_requested run_state

let exec_on_message_from_file message_path run_state =
  let message = Exec.read_message message_path in
  exec_on_message message run_state

let run_scenario scenario =
  let open Lwt_syntax in
  let kernel = scenario.kernel in
  let apply_scenario kernel =
    let rec go run_state = function
      | [] -> return run_state
      | (label, action) :: q ->
          let* tree = run_action label run_state action in
          go tree q
    in
    let* tree = Exec.initial_boot_sector_from_kernel kernel in
    let run_state = {state = tree; message_counter = 1} in
    let* _ = go run_state scenario.actions in
    return ()
  in
  Exec.run kernel apply_scenario
