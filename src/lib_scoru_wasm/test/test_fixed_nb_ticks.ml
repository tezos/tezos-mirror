(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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

(** Testing
    -------
    Component:    Wasm_pvm
    Invocation:   dune exec src/lib_scoru_wasm/test/main.exe -- --file test_fixed_nb_ticks.ml
    Subject:      WASM PVM evaluation tests for fixed nb of ticks per top level call
*)

open Wasm_utils
open Tztest_helper

let loop_module =
  {|
  (module
    (memory 1)
    (export "mem"(memory 0))
    (func (export "kernel_run")
      (loop $my_loop
        br $my_loop)
    )
  )
|}

let noop_module =
  {|
  (module
    (memory 1)
    (export "mem"(memory 0))
    (func (export "kernel_run")
      nop
    )
  )
|}

let snapshot_tick = Z.one

let input_tick = Z.one

let test_looping_kernel ~version () =
  let open Lwt_result_syntax in
  let max_nb_ticks = 5000L in

  (* This module loops indefinitely. *)
  let*! loop_module_tree =
    initial_tree ~version ~ticks_per_snapshot:max_nb_ticks loop_module
  in
  let*! loop_module_tree = eval_until_input_requested loop_module_tree in
  let*! tree_with_dummy_input = set_empty_inbox_step 0l loop_module_tree in
  let* stuck, _ = eval_until_stuck tree_with_dummy_input in
  match stuck with
  | Too_many_ticks -> return_unit
  | _ -> failwith "second: Unexpected stuck state!"

let test_noop_kernel ~version () =
  let open Lwt_result_syntax in
  let max_nb_ticks = 5000L in

  (* This module does a noop. *)
  let*! noop_module_tree =
    initial_tree ~version ~ticks_per_snapshot:max_nb_ticks noop_module
  in
  (* Eval until snapshot, which shouldn't take any tick since the default state
     is Snapshot. *)
  let*! tree_snapshotted = eval_until_input_requested noop_module_tree in
  (* Adds one input tick, part of the maximum number of ticks per toplevel
     call. *)
  let*! tree_with_dummy_input = set_empty_inbox_step 0l tree_snapshotted in
  let*! tree = eval_to_snapshot tree_with_dummy_input in
  let*! info = Wasm.get_info tree in
  (* Twice as much as max ticks, one to load the inbox, the other to execute. *)
  return (assert (Z.(info.current_tick = of_int64 Int64.(mul 2L max_nb_ticks))))

let test_stuck_in_decode_kernel ~version () =
  let open Lwt_result_syntax in
  (* The PVM needs at least [4] ticks to load the smallest inbox possible:
     - 1 for Snapshot --> Collect
     - 1 for SOL
     - 1 for EOL
     - 1 for Collect -> Padding *)
  let max_nb_ticks = 4L in

  (* This module does a noop. *)
  let*! noop_module_tree =
    initial_tree ~version ~ticks_per_snapshot:max_nb_ticks noop_module
  in
  let*! noop_module_tree = eval_until_input_requested noop_module_tree in
  (* Collect an inbox. *)
  let*! tree_with_dummy_input = set_empty_inbox_step 0l noop_module_tree in
  (* Eval one tick *)
  let* stuck, tree = eval_until_stuck tree_with_dummy_input in
  assert (stuck = Too_many_ticks) ;
  let*! info = Wasm.get_info tree in
  (* Twice as much as max ticks, one to load the inbox, the other to execute. *)
  return (assert (Z.(info.current_tick = of_int64 Int64.(mul 2L max_nb_ticks))))

let test_stuck_in_init_kernel ~version () =
  let open Lwt_result_syntax in
  let max_nb_ticks = 1000L in

  (* This module does a noop. *)
  let*! noop_module_tree =
    initial_tree ~version ~ticks_per_snapshot:max_nb_ticks noop_module
  in
  let*! noop_module_tree = eval_until_input_requested noop_module_tree in
  (* Adds one input tick, part of the maximum number of ticks per toplevel
     call. *)
  let*! tree_with_dummy_input = set_empty_inbox_step 0l noop_module_tree in
  (* go to first Init step *)
  let*! tree = eval_until_init tree_with_dummy_input in
  let*! stuck = Wasm.Internal_for_tests.is_stuck tree in
  assert (stuck = None) ;

  (* set maximum to next tick and eval one more time *)
  let*! info = Wasm.get_info tree in
  let previous_max_nb_ticks = Z.of_int64 max_nb_ticks in
  let new_max_nb_ticks =
    (* Remove the input step ticks *)
    Z.(succ (sub info.current_tick previous_max_nb_ticks))
  in
  let*! tree = Wasm.Internal_for_tests.set_max_nb_ticks new_max_nb_ticks tree in
  let*! tree = Wasm.compute_step tree in

  (* Check is_stuck and current tick *)
  let*! info = Wasm.get_info tree in
  let*! stuck = Wasm.Internal_for_tests.is_stuck tree in
  assert (stuck = Some Too_many_ticks) ;
  return
    (assert (info.current_tick = Z.add previous_max_nb_ticks new_max_nb_ticks))

let tests =
  tztests_with_all_pvms
    [
      ("nb of ticks limited", `Quick, test_looping_kernel);
      ("evaluation takes fixed nb of ticks", `Quick, test_noop_kernel);
      ("stuck in decode", `Quick, test_stuck_in_decode_kernel);
      ("stuck in init", `Quick, test_stuck_in_init_kernel);
    ]

let () =
  Alcotest_lwt.run ~__FILE__ "test lib scoru wasm" [("Max nb of ticks", tests)]
  |> Lwt_main.run
