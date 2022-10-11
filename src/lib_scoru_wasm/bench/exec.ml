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

open Test_scoru_wasm_test_helpers
open Pvm_instance

let rec eval_until_input_requested tree =
  let open Lwt_syntax in
  let* info = Wasm.get_info tree in
  match info.input_request with
  | No_input_required ->
      let* tree, _ = Wasm.compute_step_many ~max_steps:Int64.max_int tree in
      eval_until_input_requested tree
  | Reveal_required _ | Input_required -> return tree

let run kernel k =
  let open Lwt_syntax in
  let* () =
    Lwt_io.with_file ~mode:Lwt_io.Input kernel (fun channel ->
        let* kernel = Lwt_io.read channel in
        k kernel)
  in
  return_unit

let set_input_step = Wasm_utils.set_input_step

let read_message name =
  let open Tezt.Base in
  let kernel_file =
    project_root // Filename.dirname __FILE__ // "inputs" // name
  in
  read_file kernel_file

let initial_boot_sector_from_kernel ?(max_tick = 1000000000000L) kernel =
  let open Lwt_syntax in
  let+ tree = Wasm_utils.initial_tree ~max_tick ~from_binary:true kernel in
  tree
